-module(cascadae_peers).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(peers_state, {
        peers = dict:new(), %% dict(Pid => #peer{})
        session_pid,
        session_tag,
        session_mref,
        torrent_ids,
        update_table_tref,

        tid_pids = [],
        %% Loaded peers on the client side.
        %% Both visible or not.
        cl_tid_pids = [],
        viz_tid_pids = [],
        viz_torrent_ids = []
}).

-record(peer, {
    %% Dynamic
    %%  From table
    state,
    %%  From peer state
    choke_state,
    interest_state,
    local_choke,
    recv_rate,
    send_rate
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         set_torrent_list/2,
         activate/1,
         deactivate/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Session, Tag) ->
    gen_server:start_link(?MODULE, [Session, Tag], []).

set_torrent_list(Srv, TorrentIDs) ->
    gen_server:cast(Srv, {set_torrent_list, TorrentIDs}).

activate(Srv) ->
    gen_server:cast(Srv, activate).

deactivate(Srv) ->
    gen_server:cast(Srv, deactivate).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Session, Tag]) ->
    SMRef = monitor(process, Session),
    State = #peers_state{
            session_pid=Session,
            session_tag=Tag,
            session_mref=SMRef
            },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({set_torrent_list, NewTorrentIDs}, State) ->
    {noreply, set_torrent_list_int(NewTorrentIDs, State)};
handle_cast(activate, State=#peers_state{update_table_tref=undefined}) ->
    self() ! update_table,
    {ok, TRef} = timer:send_interval(5000, update_table),
    lager:info("Activate ~p.", [TRef]),
    {noreply, State#peers_state{update_table_tref=TRef}};
handle_cast(activate, State=#peers_state{}) ->
    lager:info("Skip activation."),
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=undefined}) ->
    lager:info("Skip deactivation."),
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=TRef}) ->
    lager:info("Deactivate ~p.", [TRef]),
    {ok, cancel} = timer:cancel(TRef),
    {noreply, State#peers_state{update_table_tref=undefined}}.


handle_info(update_table, State) ->
    lager:debug("Handle update_table timeout.", []),
    {noreply, cron_find_new(State)};
handle_info({'DOWN', MRef, process, _, Reason},
            State=#peers_state{session_mref=MRef}) ->
    {stop, Reason, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

render_ip({A,B,C,D}) ->
    iolist_to_binary(io_lib:format("~3..0B.~3..0B.~3..0B.~3..0B", [A,B,C,D])).


to_binary(Term) ->
    iolist_to_binary(io_lib:format("~w", [Term])).


atom_to_binary(X) -> atom_to_binary(X, utf8).



%% not active
set_torrent_list_int(NewTorrentIds, S=#peers_state{update_table_tref=undefined}) ->
    lager:info("Update torrent list ~p => ~p while updates are inactive.",
               [S#peers_state.viz_torrent_ids, NewTorrentIds]),
    S#peers_state{viz_torrent_ids=NewTorrentIds};
set_torrent_list_int(NewTorrentIds, S=#peers_state{tid_pids=OldSTP,
                                               cl_tid_pids=OldClSTP}) ->
    self() ! update_table, %% TODO: can be optimized
    lager:info("Update torrent list ~p => ~p.",
               [S#peers_state.viz_torrent_ids, NewTorrentIds]),
    NotAddedYet = ordsets:subtract(OldSTP, OldClSTP),
    S1 = S#peers_state{viz_torrent_ids=NewTorrentIds},
    viz_add_peers(NotAddedYet, S1).


cron_find_new(S=#peers_state{tid_pids=OldSTP}) ->
    {value, TP} = etorrent_table:all_tid_and_pids(),
    STP = lists:usort(TP),
    {DeletedTP, AddedTP} = cascadae_lib:ordsets_diff(OldSTP, STP),
    S1 = S#peers_state{tid_pids=STP},
    S2 = viz_delete_peers(DeletedTP, S1),
    S3 = viz_add_peers(AddedTP, S2),
    S4 = viz_diff_peers(S3),
    S4.


%% Delete rows on the client side.
viz_delete_peers([], S=#peers_state{}) ->
    S;
viz_delete_peers(DeletedTP,
                 S=#peers_state{cl_tid_pids=OldClSTP, viz_tid_pids=OldVizSTP,
                                peers=Peers}) ->
    lager:info("Deleted peers ~p.", [DeletedTP]),
    NewClSTP = ordsets:subtract(OldClSTP, DeletedTP),
    NewVizSTP = ordsets:subtract(OldVizSTP, DeletedTP),
    ClDeletedTP = ordsets:subtract(DeletedTP, OldClSTP),
    case ClDeletedTP of
        [] -> S;
        [_|_] ->
            %% Push ClDeletedTP to the client.
            lager:info("Delete ~p from the client peer table.", [ClDeletedTP]),
            PeerPids = [to_binary(Pid) || Pid <- ClDeletedTP],
            push_to_client({delete_list, PeerPids}, S),
            NewPeers = clean_peers(ClDeletedTP, Peers),
            S#peers_state{cl_tid_pids=NewClSTP, viz_tid_pids=NewVizSTP,
                          peers=NewPeers}
    end.

%% Add requested rows to the client 
viz_add_peers([], S=#peers_state{}) ->
    S;
viz_add_peers(AddedTP, S=#peers_state{cl_tid_pids=OldClSTP, viz_tid_pids=OldVizSTP,
                                      viz_torrent_ids=VizTorrents}) ->
    lager:info("Added new peers ~p.", [AddedTP]),
    %% Push VizAddedSTP to the client.
    VizAddedSTP = filter_visible_peers(AddedTP, VizTorrents),
    case VizAddedSTP of
        [] -> S;
        [_|_] ->
            NewClSTP = ordsets:union(OldClSTP, VizAddedSTP),
            lager:info("Add ~p to the client peer table.", [VizAddedSTP]),
            {List, S1} = peer_list(VizAddedSTP, S),
            case OldVizSTP of
                [_|_] -> push_to_client({add_list, List}, S);
                []    -> push_to_client({set_list, List}, S)
            end,
            NewVizSTP = ordsets:union(OldVizSTP, VizAddedSTP),
            S1#peers_state{cl_tid_pids=NewClSTP, viz_tid_pids=NewVizSTP}
    end.


push_to_client(Mess, #peers_state{ session_pid=Session, session_tag=Tag }) ->
    lager:info("Send ~p to ~p.", [{Tag, Mess}, Session]),
    cascadae_session:send(Session, {Tag, Mess}).


%% Empty list means all torrents are visible.
filter_visible_peers(AddedTP, []) ->
    AddedTP;
filter_visible_peers(AddedTP, VizTorrents) ->
    lager:info("Filter visible peers, visible torrents are ~p.", [VizTorrents]),
    cascadae_lib:orddict_with_set_intersection(AddedTP, VizTorrents).
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

peers() ->
    [{1,list_to_pid("<0.11147.1>")},{1,list_to_pid("<0.14344.15>")},
     {1,list_to_pid("<0.14357.15>")},{1,list_to_pid("<0.14366.15>")},
     {1,list_to_pid("<0.14375.15>")},{1,list_to_pid("<0.14393.15>")},
     {1,list_to_pid("<0.14402.15>")},{1,list_to_pid("<0.14410.15>")},
     {1,list_to_pid("<0.14426.15>")},{1,list_to_pid("<0.14446.15>")},
     {1,list_to_pid("<0.14456.15>")},{1,list_to_pid("<0.14467.15>")},
     {1,list_to_pid("<0.14492.15>")},{1,list_to_pid("<0.14503.15>")},
     {1,list_to_pid("<0.14635.15>")},{1,list_to_pid("<0.14973.15>")}].

filter_visible_peers_test_() ->
    [?_assertEqual(peers(), filter_visible_peers(peers(), [1]))
    ].

-endif.



peer_list(TPs, S) ->
    peer_list(TPs, S, []).

peer_list([{Tid, Pid}|TPs], S, Acc) ->
    case get_peer(Tid, Pid, S) of
        {undefined, S1} -> peer_list(TPs, S1, Acc);
        {ObjJSON, S1} -> peer_list(TPs, S1, [ObjJSON|Acc])
    end;
peer_list([], S, Acc) ->
    {lists:reverse(Acc), S}.


get_peer(Tid, Pid, S=#peers_state{peers=Peers}) ->
    case etorrent_table:get_peer({pid, Pid}) of
        {value, PL} ->
            PL2 = case etorrent_peer_states:get_peer(Tid, Pid) of
                    {value, PL2_} -> PL2_;
                    not_found     -> []
                  end,
            PLState = proplists:get_value(state, PL),
            PL2ChokeS = proplists:get_value(choke_state, PL2),
            PL2InterS = proplists:get_value(interest_state, PL2),
            PL2LocalC = proplists:get_value(local_choke, PL2),
            RecvRate = as_int(etorrent_peer_states:get_recv_rate(Tid, Pid)),
            SendRate = as_int(etorrent_peer_states:get_send_rate(Tid, Pid)),
            ObjJSON =
                [{torrent_id, Tid}
                ,{id, to_binary(Pid)}
                ,{ip, render_ip(proplists:get_value(ip, PL))}
                ,{port, proplists:get_value(port, PL)}

                ,{state, atom_to_binary(PLState)}
                ,{choke_state, atom_to_binary(PL2ChokeS)}
                ,{interest_state, atom_to_binary(PL2InterS)}
                ,{local_choke, PL2LocalC} % bool
                ,{recv_rate, RecvRate}
                ,{send_rate, SendRate}
                ],
            Peer = #peer{
                %% Dynamic
                %%  From table
                state=PLState,
                %%  From peer state
                choke_state=PL2ChokeS,
                interest_state=PL2InterS,
                local_choke=PL2LocalC,
                recv_rate=RecvRate,
                send_rate=SendRate
            },
            NewPeers = dict:store(Pid, Peer, Peers),
            {filter_undefined(ObjJSON), S#peers_state{peers=NewPeers}};
        not_found -> {undefined, S}
    end.

filter_undefined(ObjJSON) ->
    [{K,V} || {K,V} <- ObjJSON, V =/= undefined].


as_int(X) when is_float(X) -> round(X);
as_int(_) -> undefined.


clean_peers([TP|TPs], Peers) ->
    clean_peers(TPs, dict:erase(TP, Peers));
clean_peers([], Peers) ->
    Peers.


viz_diff_peers(S=#peers_state{viz_tid_pids=[]}) -> S;
viz_diff_peers(S=#peers_state{viz_tid_pids=VizSTP, peers=Peers}) ->
    {Diff, NewPeers} = diff_peers(VizSTP, Peers, []),
    case Diff of
        [] -> S;
        [_|_] ->
            lager:info("Diff ~p", [Diff]),
            push_to_client({diff_list, Diff}, S),
            S#peers_state{peers=NewPeers}
    end.

diff_peers([{Tid,Pid}|TPs], Peers, Diffs) ->
    case dict:find(Pid, Peers) of
        {ok, OldPeer} ->
            case diff_peer(Tid, Pid, OldPeer) of
                {changed, Diff, NewPeer} ->
                    diff_peers(TPs, dict:store(Pid, NewPeer, Peers),
                               [Diff|Diffs]);
                same ->
                    diff_peers(TPs, Peers, Diffs)
            end;

        error ->
            diff_peers(TPs, Peers, Diffs)
    end;
diff_peers([], Peers, Diffs) ->
    {Diffs, Peers}.



diff_peer(Tid, Pid, OldPeer) ->
    case etorrent_table:get_peer({pid, Pid}) of
        {value, PL} ->
            PL2 = case etorrent_peer_states:get_peer(Tid, Pid) of
                    {value, PL2_} -> PL2_;
                    not_found     -> []
                  end,
            PLState = proplists:get_value(state, PL),
            PL2ChokeS = proplists:get_value(choke_state, PL2),
            PL2InterS = proplists:get_value(interest_state, PL2),
            PL2LocalC = proplists:get_value(local_choke, PL2),
            RecvRate = as_int(etorrent_peer_states:get_recv_rate(Tid, Pid)),
            SendRate = as_int(etorrent_peer_states:get_send_rate(Tid, Pid)),
            NewPeer = #peer{
                %% Dynamic
                %%  From table
                state=PLState,
                %%  From peer state
                choke_state=PL2ChokeS,
                interest_state=PL2InterS,
                local_choke=PL2LocalC,
                recv_rate=RecvRate,
                send_rate=SendRate
            },
            case NewPeer of
                OldPeer -> same;
                _ ->
                    #peer{
                        %% Dynamic
                        %%  From table
                        state=OldPLState,
                        %%  From peer state
                        choke_state=OldPL2ChokeS,
                        interest_state=OldPL2InterS,
                        local_choke=OldPL2LocalC,
                        recv_rate=OldRecvRate,
                        send_rate=OldSendRate
                    } = OldPeer,
                    Diff = [{id, to_binary(Pid)}]
                    ++ if PLState =:= OldPLState -> [];
                          true -> [{state, atom_to_binary(PLState)}]
                       end
                    ++ if PL2ChokeS =:= OldPL2ChokeS -> [];
                          true -> [{choke_state, atom_to_binary(PL2ChokeS)}]
                       end
                    ++ if PL2InterS =:= OldPL2InterS -> [];
                          true -> [{interest_state, atom_to_binary(PL2InterS)}]
                       end
                    ++ if PL2LocalC =:= OldPL2LocalC -> [];
                          true -> [{local_choke, PL2LocalC}]
                       end
                    ++ if RecvRate =:= OldRecvRate -> [];
                          true -> [{recv_rate, RecvRate}]
                       end
                    ++ if SendRate =:= OldSendRate -> [];
                          true -> [{send_rate, SendRate}]
                       end,
                    {changed, Diff, NewPeer}
            end;
        not_found -> same
    end.

