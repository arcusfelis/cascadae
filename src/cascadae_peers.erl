-module(cascadae_peers).


-export([all_peers/0]).

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
            El = zip(PL, 'undefined'),
            zip_list(PLT, [PLS|PLST], [El|Acc]);

        Pid1 > Pid2 -> 
            El = zip('undefined', PLS),
            zip_list([PL|PLT], PLST, [El|Acc])
    end;

zip_list([], [PLS|PLST], Acc) ->
    El = zip('undefined', PLS),
    zip_list([], PLST, [El|Acc]);

zip_list([PL|PLT], [], Acc) ->
    El = zip(PL, 'undefined'),
    zip_list(PLT, [], [El|Acc]);

zip_list([], [], Acc) ->
    lists:reverse(Acc).



zip(PL, 'undefined') ->
    Pid = proplists:get_value(pid, PL),

    [{'id', to_binary(Pid)}

    ,{'ip', render_ip(proplists:get_value('ip', PL))}
    ,{'port', proplists:get_value('port', PL)}
    ,{'torrent_id', proplists:get_value('torrent_id', PL)}
    ,{'state', atom_to_binary(proplists:get_value('state', PL))}
    ];

zip('undefined', PLS) ->
    Pid = proplists:get_value(pid, PLS),

    [{'id', to_binary(Pid)}

    ,{'choke_state', atom_to_binary(proplists:get_value('choke_state', PLS))}
    ,{'interest_state', atom_to_binary(proplists:get_value('interest_state', PLS))}
    ,{'local_choke', proplists:get_value('local_choke', PLS)} % bool
    ];

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

