-module(cascadae_peers).
-export([all_peers/0]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(peers_state, {
        session_pid,
        session_tag,
        torrent_ids,
        update_table_tref
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
    TRef = timer:send_interval(5000, update_table),
    State = #peers_state{
            session_pid=Session,
            session_tag=Tag,
            update_table_tref=TRef
            },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(activate, State=#peers_state{update_table_tref=undefined}) ->
    {ok, TRef} = timer:send_interval(5000, update_table),
    {noreply, State#peers_state{update_table_tref=TRef}};
handle_cast(activate, State=#peers_state{}) ->
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=undefined}) ->
    {noreply, State};
handle_cast(deactivate, State=#peers_state{update_table_tref=TRef}) ->
    ok = timer:cancel(TRef),
    {noreply, State#peers_state{update_table_tref=undefined}}.


handle_info(update_table, State) ->
    lager:debug("Handle update_table timeout.", []),
    #peers_state{
        torrent_ids=TorrentIDs,
        session_pid=Session,
        session_tag=Tag
        } = State,
%   cascadae_session:send(Session, {Tag, {diff_list, TorrentID, Diff}})
    {noreply, State#peers_state{}}.

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

