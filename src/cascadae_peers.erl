-module(cascadae_peers).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(peers_state, {
        session_pid,
        session_tag,
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
    %% static fields
    pid,
    torrent_id,

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
    {ok, TRef} = timer:send_interval(5000, update_table),
    State = #peers_state{
            session_pid=Session,
            session_tag=Tag,
            update_table_tref=TRef
            },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({set_torrent_list, NewTorrentIDs}, State) ->
    {noreply, set_torrent_list_int(NewTorrentIDs, State)};
handle_cast(activate, State=#peers_state{update_table_tref=undefined}) ->
    {ok, TRef} = timer:send_interval(5000, update_table),
    lager:info("Activate ~p.", [TRef]),
    {noreply, State#peers_state{update_table_tref=TRef}};
handle_cast(activate, State=#peers_state{}) ->
    lager:info("Skip activation.", []),
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=undefined}) ->
    lager:info("Skip deactivation.", []),
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=TRef}) ->
    lager:info("Deactivate ~p.", [TRef]),
    {ok, cancel} = timer:cancel(TRef),
    {noreply, State#peers_state{update_table_tref=undefined}}.


handle_info(update_table, State) ->
    lager:debug("Handle update_table timeout.", []),
    {noreply, cron_find_new(State)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------









%% etorrent_peer_states:get_send_rate(Id, Pid)
%% etorrent_peer_states:get_recv_rate(Id, Pid)


all_peers() ->
%{pid, Pid}, {ip, IP}, {port, Port},
%{torrent_id, TorrentId},
%{state, State}
    UnsortedPLs = etorrent_table:all_peers(),
    SortedPLs = sort_proplists(UnsortedPLs),

%{pid, Pid},
%{torrent_id, TorrentId},
%{choke_state, Chokestate},
%{interest_state, Intereststate},
%{local_choke, Localchoke}
    UnsortedPLSs = etorrent_peer_states:all_peers(),
    SortedPLSs = sort_proplists(UnsortedPLSs),

    zip_list(SortedPLs, SortedPLSs, []).


sort_proplists(PL) ->
    lists:sort(fun(X, Y) -> 
        VX = proplists:get_value(pid, X), 
        VY = proplists:get_value(pid, Y),
        VX < VY
        end, PL).


%% @doc This is the realization of lists:zipwith/3 for incomplete lists.
zip_list([PL|PLT], [PLS|PLST], Acc) ->
    Pid1 = proplists:get_value(pid, PL),
    Pid2 = proplists:get_value(pid, PLS),

    if
        Pid1 =:= Pid2 -> 
            El = zip(PL, PLS),
            zip_list(PLT, PLST, [El|Acc]);

        Pid1 < Pid2 -> 
            El = zip(PL, undefined),
            zip_list(PLT, [PLS|PLST], [El|Acc]);

        Pid1 > Pid2 -> 
            El = zip(undefined, PLS),
            zip_list([PL|PLT], PLST, [El|Acc])
    end;

zip_list([], [PLS|PLST], Acc) ->
    El = zip(undefined, PLS),
    zip_list([], PLST, [El|Acc]);

zip_list([PL|PLT], [], Acc) ->
    El = zip(PL, undefined),
    zip_list(PLT, [], [El|Acc]);

zip_list([], [], Acc) ->
    lists:reverse(Acc).



zip(PL, undefined) ->
    Pid = proplists:get_value(pid, PL),

    [{id, to_binary(Pid)}

    ,{ip, render_ip(proplists:get_value(ip, PL))}
    ,{port, proplists:get_value(port, PL)}
    ,{torrent_id, proplists:get_value(torrent_id, PL)}
    ,{state, atom_to_binary(proplists:get_value(state, PL))}
    ];

zip(undefined, PLS) ->
    Pid = proplists:get_value(pid, PLS),

    [{id, to_binary(Pid)}

    ,{choke_state, atom_to_binary(proplists:get_value(choke_state, PLS))}
    ,{interest_state, atom_to_binary(proplists:get_value(interest_state, PLS))}
    ,{local_choke, proplists:get_value(local_choke, PLS)} % bool
    ];

zip(PL, PLS) ->
    Pid = proplists:get_value(pid, PL),
    Pid = proplists:get_value(pid, PLS),

    [{id, to_binary(Pid)}

    ,{ip, render_ip(proplists:get_value(ip, PL))}
    ,{port, proplists:get_value(port, PL)}
    ,{torrent_id, proplists:get_value(torrent_id, PL)}
    ,{state, atom_to_binary(proplists:get_value(state, PL))}

    ,{choke_state, atom_to_binary(proplists:get_value(choke_state, PLS))}
    ,{interest_state, atom_to_binary(proplists:get_value(interest_state, PLS))}
    ,{local_choke, proplists:get_value(local_choke, PLS)} % bool
    ].


render_ip({A,B,C,D}) ->
    iolist_to_binary(io_lib:format("~w.~w.~w.~w", [A,B,C,D])).


to_binary(Term) ->
    iolist_to_binary(io_lib:format("~w", [Term])).


atom_to_binary(X) -> atom_to_binary(X, utf8).





%% not active
set_torrent_list_int(NewTorrentIds, S=#peers_state{update_table_tref=undefined}) ->
    lager:info("Update torrent list ~p => ~p while updates are inactive.",
               [S#peers_state.viz_torrent_ids, NewTorrentIds]),
    S#peers_state{viz_torrent_ids=NewTorrentIds};
set_torrent_list_int(NewTorrentIds, S=#peers_state{tid_pids=OldSTP,
                                               cl_tid_pids=OldClSTP,
                                               viz_tid_pids=OldVizSTP}) ->
    self() ! update_table, %% TODO: can be optimized
    lager:info("Update torrent list ~p => ~p.",
               [S#peers_state.viz_torrent_ids, NewTorrentIds]),
    NotAddedYet = ordsets:subtract(OldSTP, OldClSTP),
    S1 = S#peers_state{viz_torrent_ids=NewTorrentIds},
    viz_add_peers(NotAddedYet, S1).


cron_find_new(S=#peers_state{tid_pids=OldSTP}) ->
    {value, TP} = etorrent_table:all_tid_and_pids(),
    STP = lists:usort(TP),
    {DeletedTP, AddedTP} = ordsets_diff(OldSTP, STP),
    S1 = S#peers_state{tid_pids=STP},
    S2 = viz_delete_peers(DeletedTP, S1),
    S3 = viz_add_peers(AddedTP, S2),
    S3.


%% Delete rows on the client side.
viz_delete_peers([], S=#peers_state{}) ->
    S;
viz_delete_peers(DeletedTP, S=#peers_state{cl_tid_pids=OldClSTP, viz_tid_pids=OldVizSTP}) ->
    lager:info("Deleted peers ~p.", [DeletedTP]),
    NewClSTP = ordsets:subtract(OldClSTP, DeletedTP),
    NewVizSTP = ordsets:subtract(OldVizSTP, DeletedTP),
    ClDeletedTP = ordsets:subtract(DeletedTP, OldClSTP),
    case ClDeletedTP of
        [] -> S;
        [_|_] ->
            %% Push ClDeletedTP to the client.
            lager:info("Delete ~p from the client peer table.", [ClDeletedTP]),
            PeerPids = [Pid || Pid <- ClDeletedTP],
            push_to_client({delete_list, PeerPids}, S),
            S#peers_state{cl_tid_pids=NewClSTP, viz_tid_pids=NewVizSTP}
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
            push_to_client({add_list, List}, S),
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
    orddict_with_set_intersection(AddedTP, VizTorrents).

orddict_with_set_intersection([{K,V}|Dict], [K|Set]) ->
    [{K,V}|orddict_with_set_intersection(Dict, Set)];
orddict_with_set_intersection([{DK,V}|Dict], [SK|Set]) when DK < SK ->
    orddict_with_set_intersection(Dict, [SK|Set]);
orddict_with_set_intersection([{DK,V}|Dict], [_|Set]) ->
     orddict_with_set_intersection({DK,V}, Set);
orddict_with_set_intersection(_, _) ->
     [].



ordsets_diff(Olds, News) ->
    ordsets_diff(Olds, News, [], []).

ordsets_diff([X|Olds], [X|News], Added, Deleted) ->
    ordsets_diff(Olds, News, Added, Deleted);
ordsets_diff([Old|Olds], [New|News], Added, Deleted) when Old < New ->
    ordsets_diff(Olds, [New|News], Added, [Old|Deleted]);
ordsets_diff([Old|Olds], [New|News], Added, Deleted) ->
    ordsets_diff([Old|Olds], News, [New|Added], Deleted);
ordsets_diff(Olds, News, Added, Deleted) ->
    {lists:reverse(Deleted, Olds), lists:reverse(Added, News)}.
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

    
ordsets_diff_test_() ->
    [?_assertEqual({[1],[]},       ordsets_diff([1,2,3], [2,3]))
    ,?_assertEqual({[],[1,5]},     ordsets_diff([2,3], [1,2,3,5]))
    ,?_assertEqual({[],[1,5,6,7]}, ordsets_diff([2,3], [1,2,3,5,6,7]))
    ,?_assertEqual({[],[1,2,3]},   ordsets_diff([], [1,2,3]))
    ,?_assertEqual({[1,2,3],[]},   ordsets_diff([1,2,3], []))
    ,?_assertEqual({[4],[1,5]},    ordsets_diff([2,3,4], [1,2,3,5]))
    ].

orddict_with_set_intersection_test_() ->
    [?_assertEqual([{2,b}],        orddict_with_set_intersection([{1,a},{2,b}], [2,3]))
    ,?_assertEqual([{1,a},{2,b}],  orddict_with_set_intersection([{1,a},{2,b}], [1,2,3]))
    ,?_assertEqual([],             orddict_with_set_intersection([{1,a},{2,b}], []))
    ,?_assertEqual([],             orddict_with_set_intersection([], [2,3]))
    ,?_assertEqual([],             orddict_with_set_intersection([{4,d}], [2,3]))
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


get_peer(Tid, Pid, S) ->
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
            RecvRate = etorrent_peer_states:get_recv_rate(Tid, Pid),
            SendRate = etorrent_peer_states:get_send_rate(Tid, Pid),
            ObjJSON =
                [{torrent_id, Tid}
                ,{id, to_binary(Pid)}
                ,{ip, render_ip(proplists:get_value(ip, PL))}
                ,{port, proplists:get_value(port, PL)}
                ,{torrent_id, proplists:get_value(torrent_id, PL)}

                ,{state, atom_to_binary(PLState)}
                ,{choke_state, atom_to_binary(PL2ChokeS)}
                ,{interest_state, atom_to_binary(PL2InterS)}
                ,{local_choke, PL2LocalC} % bool
                ,{recv_rate, as_int(RecvRate)}
                ,{send_rate, as_int(SendRate)}
                ],
            #peer{
                %% static fields
                pid=Pid,
                torrent_id=Tid,

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
            {filter_undefined(ObjJSON), S};
        not_found -> {undefined, S}
    end.

filter_undefined(ObjJSON) ->
    [{K,V} || {K,V} <- ObjJSON, V =/= undefined].


as_int(X) when is_float(X) -> round(X);
as_int(_) -> undefined.
