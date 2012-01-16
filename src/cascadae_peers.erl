-module(cascadae_peers).


-export([all_peers/0]).

all_peers() ->
%{pid, Pid}, {ip, IP}, {port, Port},
%{torrent_id, TorrentId},
%{state, State}
    UnsortedPLs = etorrent_table:all_peers(),
    SortedPSs = sort_proplists(UnsortedPLs),

%{pid, Pid},
%{torrent_id, TorrentId},
%{choke_state, Chokestate},
%{interest_state, Intereststate},
%{local_choke, Localchoke}
    UnsortedPLSs = etorrent_peer_states:all_peers(),
    SortedPLSs = sort_proplists(UnsortedPLSs),

    lists:zipwith(fun zip/2, SortedPSs, SortedPLSs).


sort_proplists(PL) ->
    lists:sort(fun(X, Y) -> 
        VX = proplists:get_value(pid, X), 
        VY = proplists:get_value(pid, Y),
        VX < VY
        end, PL).


zip(PL, PLS) ->
    Pid = proplists:get_value(pid, PL),
    Pid = proplists:get_value(pid, PLS),
    [{'id', to_binary(Pid)}
    ,{'ip', render_ip(proplists:get_value('ip', PL))}
    ,{'port', proplists:get_value('port', PL)}
    ,{'torrent_id', proplists:get_value('torrent_id', PL)}
    ,{'state', atom_to_binary(proplists:get_value('state', PL))}

    ,{'choke_state', atom_to_binary(proplists:get_value('choke_state', PLS))}
    ,{'interest_state', atom_to_binary(proplists:get_value('interest_state', PLS))}
    ,{'local_choke', proplists:get_value('local_choke', PLS)} % bool
    ].

render_ip({A,B,C,D}) ->
    list_to_binary(io_lib:format("~w.~w.~w.~w", [A,B,C,D])).


to_binary(Term) ->
    list_to_binary(io_lib:format("~w", [Term])).


atom_to_binary(X) -> list_to_binary(atom_to_list(X)).
