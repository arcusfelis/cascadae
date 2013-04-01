-module(cascadae_lib).
-export([ordsets_diff/2,
         orddict_with_set_intersection/2]).

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


%% Returns values from ordered bag (set with non-unique keys) Dict
%% in which values are listened in ordered Set.
orddict_with_set_intersection([{K,V}|Dict], [K|_]=Set) ->
    [{K,V}|orddict_with_set_intersection(Dict, Set)];
orddict_with_set_intersection([{DK,_V}|Dict], [SK|_]=Set) when DK < SK ->
    %% Skip {DK,V}
    orddict_with_set_intersection(Dict, Set);
orddict_with_set_intersection([_|_]=Dict, [_|Set]) ->
    %% Skip SK
     orddict_with_set_intersection(Dict, Set);
orddict_with_set_intersection(_, _) ->
     [].

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
    ,?_assertEqual([{1,a},{1,aa},{2,b}],
                   orddict_with_set_intersection([{1,a},{1,aa},{2,b}], [1,2,3]))
    ].

-endif.
