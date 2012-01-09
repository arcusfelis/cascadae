-module(c_etorrent_rpc).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get_entire_torrent_list/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).


-type torrent_id() :: integer().


% There is four different formats of torrent.
-type etorrent_pl()  :: [{atom(), term()}].
-type etorrent_pl2() :: [{atom(), term()}].

-type json_pl() :: [{atom(), term()}].

-record(torrent, {
    'id'         :: torrent_id(),
    'left'       :: integer(),
    'leechers'   :: integer(),
    'seeders'    :: integer(),
    'all_time_downloaded' :: integer(),
    'all_time_uploaded'   :: integer(),
    'downloaded' :: integer(),
    'uploaded'   :: integer(),
    'state'      :: atom(),

    'speed_in'  = 0 :: integer(),
    'speed_out' = 0 :: integer()
}).

% {id,1},
% {total,9654049623},
% {left,0},
% {uploaded,4967590},
% {downloaded,5531},
% {all_time_downloaded,9975728691},
% {all_time_uploaded,878062405},
% {leechers,13},
% {seeders,102},
% {state,seeding}


-type torrent_diff() :: [
          {'id', torrent_id()} 
        | {'left', integer()} 
        | {'leechers', integer()}
        | {'seeders', integer()}
    ].


-record(diff_acc, {
    'diff'=[] :: [torrent_diff()],
    'added'=[] :: [torrent_id()],
    'deleted'=[] :: [torrent_id()]
}).


-record(state, {
    config :: [{atom(), term()}],
    remote_node :: node(),
    timer :: reference(),
    torrents :: [#torrent{}],
    handler :: pid(),
    tick :: integer()
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Handler = self(),
    Config = [{'node', 'etorrent@127.0.0.1'}
             ,{'cookie', 'etorrent'}
             ,{'refresh_interval', 2000} % in ms
             ],
    Args = [Handler, Config],
    gen_server:start_link(?MODULE, Args, []).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Handler, Config]) ->
    RemoteNode = proplists:get_value('node', Config),
    Tick = proplists:get_value('refresh_interval', Config),

    % Connect to the etorrent node
    case proplists:get_value('cookie', Config) of
    'undefined' ->
        ok;
    Cookie ->
        erlang:set_cookie(RemoteNode, Cookie)
    end,
    true = net_kernel:connect_node(RemoteNode),

    {ok, TRef} = timer:send_interval(Tick, 'tick'),

    PLs = torrent_list_rpc_call(RemoteNode),
    UnsortedNewTorrents = lists:map(
        fun etorrent_pl_to_list_of_records/1,
        PLs),
    NewTorrents = sort_records(UnsortedNewTorrents),

    State = #state{
        config = Config,
        remote_node = RemoteNode,
        timer = TRef,
        handler = Handler,
        torrents = NewTorrents,
        tick = Tick
    },

    {ok, State}.


handle_call('get_entire_torrent_list', _From, State) ->
    Reply = torrent_list_rpc(State#state.remote_node),
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Search in the list using fun(X) -> boolean().
search([H|T], F) ->
    case F(H) of
    true -> H;
    false -> search(T, F)
    end;
search([], _F) ->
    false.


handle_info('tick'=_Info, State=#state{
        torrents=OldTorrents, 
        remote_node=RemoteNode,
        handler=Handler,
        tick=Tick}) ->
    % proplists from etorrent.
    PLs = torrent_list_rpc_call(RemoteNode),
    UnsortedNewTorrents = lists:map(
        fun etorrent_pl_to_list_of_records/1,
        PLs),
    NewTorrents = sort_records(UnsortedNewTorrents),


    NewTorrents2 = calc_speed_records(OldTorrents, NewTorrents, Tick),

    #diff_acc{
        diff=Diff, 
        added=Added, 
        deleted=Deleted} = diff_records(OldTorrents, NewTorrents2),

    case Diff of
    [] -> 'skip';
    _  -> 
        % TODO: fix the direct message passing
        Handler ! {'diff_list', Diff}
    end,

    case Added of
    [] -> 'skip';
    _  -> 
        Fn = form_json_proplist_fn(RemoteNode),

        % Convert the list of ids to list of JSON.
        AddedJSON = lists:map(fun(Id) ->
                Fn2 = fun(X) -> 
                    lists:member({'id', Id}, X) 
                    end,
                PL = search(PLs, Fn2),
                Fn(PL)
                end, Added),

        % TODO: fix the direct message passing
        Handler ! {'add_list', AddedJSON}
    end,

    case Deleted of
    [] -> 'skip';
    _  -> 
        % TODO: fix the direct message passing
        Handler ! {'delete_list', Deleted}
    end,

    {noreply, State#state{torrents=NewTorrents2}}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec get_entire_torrent_list(pid()) -> [json_pl()].
get_entire_torrent_list(Server) ->
    gen_server:call(Server, 'get_entire_torrent_list').



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec torrent_list_rpc(node()) -> [json_pl()].
torrent_list_rpc(RemoteNode) ->
    List = torrent_list_rpc_call(RemoteNode),
    lists:map(form_json_proplist_fn(RemoteNode), List).


-spec torrent_list_rpc_call(node()) -> [etorrent_pl()].
torrent_list_rpc_call(RemoteNode) ->
    rpc:call(RemoteNode, etorrent_query, torrent_list, []).


%% @doc Returns HOF.
%%      The HOF converts a proplist from etorrent_query:torrent_list 
%%      to a proplist for JSON.
%% @end
form_json_proplist_fn(RemoteNode) ->
    fun(X) ->
        Id = proplists:get_value('id', X),
        PL = torrent_rpc(RemoteNode, Id),
        Name = proplists:get_value('filename', PL),

        [{'id',         Id}
        ,{'name',       list_to_binary(Name)}
        ,{'total',      proplists:get_value('total', X)}
        ,{'left',       proplists:get_value('left', X)}
        ,{'online',    'false'}
        ,{'leechers',   proplists:get_value('leechers', X)}
        ,{'seeders',    proplists:get_value('seeders', X)}
        ,{'state',      atom_to_binary(proplists:get_value('state', X))}
        ,{'downloaded', proplists:get_value('downloaded', X)}
        ,{'uploaded',   proplists:get_value('uploaded', X)}
        ,{'all_time_downloaded',  
                        proplists:get_value('all_time_downloaded', X)}
        ,{'all_time_uploaded',  
                        proplists:get_value('all_time_uploaded', X)}
        ]
    end.


-spec torrent_rpc(node(), torrent_id()) -> etorrent_pl2().
torrent_rpc(RemoteNode, Id) ->
    {value, PL} = rpc:call(RemoteNode, etorrent_table, get_torrent, [Id]),
    PL.

-spec etorrent_pl_to_list_of_records(etorrent_pl()) -> #torrent{}.
etorrent_pl_to_list_of_records(X) ->
    #torrent{
        id       = proplists:get_value('id', X),
        left     = proplists:get_value('left', X),
        leechers = proplists:get_value('leechers', X),
        seeders  = proplists:get_value('seeders', X),
        state    = proplists:get_value('state', X),
        downloaded = proplists:get_value('downloaded', X),
        uploaded   = proplists:get_value('uploaded', X),
        all_time_downloaded = proplists:get_value('all_time_downloaded', X),
        all_time_uploaded   = proplists:get_value('all_time_uploaded', X)
     }.


%% @doc Sort a list by id.
-spec sort_records([#torrent{}]) -> [#torrent{}].
sort_records(List) ->
    ComparatorFn = fun(X, Y) ->
            X#torrent.id > Y#torrent.id
        end,
    lists:sort(ComparatorFn, List).


calc_speed_records(Olds, News, Tick) ->
    FU = fun(#torrent{uploaded=X, speed_out=0}, 
             #torrent{uploaded=X, speed_out=0}=New) -> New;
            (#torrent{uploaded=X}, 
             #torrent{uploaded=X}=New) -> New#torrent{speed_out=0};
            (#torrent{uploaded=O}, 
             #torrent{uploaded=N}=New) -> 
                New#torrent{speed_out=calc_speed(O, N, Tick)}
        end,

    FD = fun(#torrent{downloaded=X, speed_in=0}, 
             #torrent{downloaded=X, speed_in=0}=New) -> New;
            (#torrent{downloaded=X}, 
             #torrent{downloaded=X}=New) -> New#torrent{speed_in=0};
            (#torrent{downloaded=O}, 
             #torrent{downloaded=N}=New) -> 
                New#torrent{speed_in=calc_speed(O, N, Tick)}
        end,

    F = fun(Old, New) -> FU(Old, FD(Old, New)) end,
    map_records(F, Olds, News).


calc_speed(Old, New, Interval) ->
    Bytes = New - Old,
    Seconds = Interval / 1000,
    Bytes / Seconds.

%% @doc Tip: New torrents will be unchanged.
map_records(F, Olds, News) ->
    map_records(F, Olds, News, []).

map_records(F, [Old=#torrent{id=Id} | OldT], 
               [New=#torrent{id=Id} | NewT], Acc) ->
    NewAcc = [F(Old, New)|Acc],
    map_records(F, OldT, NewT, NewAcc);

% Element Old was deleted.
map_records(F, [#torrent{id=OldId} | OldT], 
               [#torrent{id=NewId} | _] = NewT, Acc) 
    when NewId>OldId ->
    map_records(F, OldT, NewT, Acc);

% Element New was added.
map_records(F, OldT, 
               [New | NewT], Acc) -> % Copy new torrent.
    map_records(F, OldT, NewT, [New | Acc]);

map_records(_F, _OldLeft, _NewLeft, Acc) ->
    lists:reverse(Acc).
    


diff_records(Olds, News) ->
    diff_records(Olds, News, #diff_acc{}).

%% @doc Calculate the difference beetween two sets of torrents. 
%%
%%      Note: Both lists are sorted.
-spec diff_records([#torrent{}], [#torrent{}]) -> #diff_acc{}.
diff_records([Old=#torrent{id=Id} | OldT], 
             [New=#torrent{id=Id} | NewT], Acc) ->

    % Compare elements with the same Id.
    case diff_element(Old, New) of
    false -> diff_records(OldT, NewT, Acc);
    Diff  -> 
        L1 = Acc#diff_acc.diff,
        L2 = [Diff | L1],
        diff_records(OldT, NewT, Acc#diff_acc{diff=L2})
    end;

% Example: Element with Id=2 was deleted:
%          Olds: 1,__OldId=2__,3
%          News: 1,__NewId=3__
diff_records([#torrent{id=OldId} | OldT], 
             [#torrent{id=NewId} | _] = NewT, Acc) when NewId>OldId ->
    L1 = Acc#diff_acc.deleted,
    L2 = [OldId | L1],
    diff_records(OldT, NewT, Acc#diff_acc{deleted=L2});

% Element New was added.
diff_records(OldT, 
             [#torrent{id=NewId} | NewT], Acc) ->
    L1 = Acc#diff_acc.added,
    L2 = [NewId | L1],
    diff_records(OldT, NewT, Acc#diff_acc{added=L2});

% Element Old was deleted.
diff_records([#torrent{id=OldId} | OldT], 
             [], Acc) ->
    L1 = Acc#diff_acc.deleted,
    L2 = [OldId | L1],
    diff_records(OldT, [], Acc#diff_acc{deleted=L2});

diff_records([], 
             [], Acc) ->
    Acc.



%% @doc Compare two states of the #torrent{} with same id.
-spec diff_element(#torrent{}, #torrent{}) -> [torrent_diff()].
diff_element(Old=#torrent{left=ORem, leechers=OLs, seeders=OSs},
             New=#torrent{left=NRem, leechers=NLs, seeders=NSs}) ->

    #torrent{uploaded=OU, downloaded=OD, state=OS}=Old,
    #torrent{uploaded=NU, downloaded=ND, state=NS}=New,
    #torrent{all_time_uploaded=OATU, all_time_downloaded=OATD}=Old,
    #torrent{all_time_uploaded=NATU, all_time_downloaded=NATD}=New,
    #torrent{speed_in=OSI, speed_out=OSO}=Old,
    #torrent{speed_in=NSI, speed_out=NSO}=New,

    UnfilteredList = 
    [case ORem of
     NRem -> 'not_modified';
     X    -> {'left', X}
     end
    ,case OLs of
     NLs  -> 'not_modified';
     X    -> {'leechers', X}
     end
    ,case OSs of
     NSs  -> 'not_modified';
     X    -> {'seeders', X}
     end
    ,case OD of
     ND   -> 'not_modified';
     X    -> {'downloaded', X}
     end
    ,case OU of
     NU   -> 'not_modified';
     X    -> {'uploaded', X}
     end
    ,case OATD of
     NATD -> 'not_modified';
     X    -> {'all_time_downloaded', X}
     end
    ,case OATU of
     NATU -> 'not_modified';
     X    -> {'all_time_uploaded', X}
     end
    ,case OS of
     NS   -> 'not_modified';
     X    -> {'state', atom_to_binary(X)}
     end
    ,case OSO of
     NSO  -> 'not_modified';
     X    -> {'speed_out', X} % Byte per second
     end
    ,case OSI of
     NSI  -> 'not_modified';
     X    -> {'speed_in', X}
     end
    ],

    % Filtering and adding id
    case [Y || Y <- UnfilteredList, Y =/=  'not_modified'] of
    [] -> false;
    FilteredList -> [{'id', New#torrent.id} | FilteredList]
    end.


atom_to_binary(X) -> list_to_binary(atom_to_list(X)).
