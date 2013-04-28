-module(cascadae_hub).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([all_torrents/0,
        add_handler/0,
        resume_handler/1,
        suspend_handler/0,
        fire_event/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1,
        handle_info/3,
        terminate/3,
        code_change/4,

        active/2,
        active/3,
        await/2,
        await/3]).


-define(HANDLER_MODULE, cascadae_session).
-define(SERVER, ?MODULE).

-type torrent_id() :: integer().


% There are 4 different formats of torrent.
-type etorrent_pl()  :: [{atom(), term()}].
-type etorrent_pl2() :: [{atom(), term()}].

-type json_pl() :: [{atom(), term()}].

-record(torrent, {
    id         :: torrent_id(),
    wanted     :: non_neg_integer(),
    left       :: non_neg_integer(),
    leechers   :: non_neg_integer(),
    seeders    :: non_neg_integer(),
    all_time_downloaded :: non_neg_integer(),
    all_time_uploaded   :: non_neg_integer(),
    downloaded :: non_neg_integer(),
    uploaded   :: non_neg_integer(),
    state      :: atom(),

    speed_in  = 0 :: non_neg_integer(),
    speed_out = 0 :: non_neg_integer()
}).



-type torrent_diff() :: [
          {id, torrent_id()} 
        | {left, integer()} 
        | {leechers, integer()}
        | {seeders, integer()}
        | {atom(), term()}
    ].


-record(diff_acc, {
    diff=[] :: [torrent_diff()],
    added=[] :: [torrent_id()],
    deleted=[] :: [torrent_id()]
}).


-record(state, {
    timer :: reference(),
    torrents :: [#torrent{}] | undefined,
    handlers = [] :: [pid()],
    tick :: integer()
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Args = [2000],
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Args, []).



%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Timeout]) ->

    SD = #state{
        tick = Timeout,
        handlers = []
    },

    {ok, await, SD}.




% Nobody cares about this event.
await({log_event, _Mess}, SD) ->
    {next_state, await, SD}.


% Initialization after "sleeping mode".
await(add_handler, {Pid, _Tag}, SD=#state{tick=Timeout}) ->
    lager:info("Add the first client on ~w.", [Pid]),

    % Registration of the client
    erlang:monitor(process, Pid),
    
    % Subscribe on new events.
    cascadae_event:add(),

    % Collect data
    PLs = query_torrent_list(),
    UnsortedNewTorrents = lists:map(fun to_record/1, PLs),
    NewTorrents = sort_records(UnsortedNewTorrents),

    % Run timer
    TRef = gen_fsm:send_event_after(Timeout, update),
    SD1 = SD#state{timer=TRef, 
                handlers=[Pid],
                torrents=NewTorrents},

    {reply, ok, active, SD1};
await({resume_handler, OldTorrents},
      {Pid, _Tag}, SD=#state{tick=Timeout}) ->
    lager:info("Add the first client on ~w.", [Pid]),

    % Registration of the client
    erlang:monitor(process, Pid),
    
    % Subscribe on new events.
    cascadae_event:add(),

    % Collect data
    PLs = query_torrent_list(),
    UnsortedNewTorrents = lists:map(fun to_record/1, PLs),
    NewTorrents = sort_records(UnsortedNewTorrents),
    send_diff_records([Pid], OldTorrents, NewTorrents, PLs),

    % Run timer
    TRef = gen_fsm:send_event_after(Timeout, update),
    SD1 = SD#state{timer=TRef, 
                handlers=[Pid],
                torrents=NewTorrents},

    {reply, ok, active, SD1}.


%% TODO: move it from here
active({log_event, Mess}, SD=#state{handlers=Handlers}) ->

    PL = event_to_json(Mess),

    lists:map(fun(H) ->
        ?HANDLER_MODULE:send(H, {log_event, PL})
        end, Handlers),

    {next_state, active, SD};

% It is called because gen_fsm:send_event_after.
active(update, SD=#state{
        torrents=OldTorrents, 
        handlers=Handlers,
        tick=Timeout}) ->

    % proplists from etorrent.
    PLs = query_torrent_list(),
    UnsortedNewTorrents = lists:map(fun to_record/1, PLs),
    NewTorrents = sort_records(UnsortedNewTorrents),
    NewTorrents2 = calc_speed_records(OldTorrents, NewTorrents, Timeout),
    send_diff_records(Handlers, OldTorrents, NewTorrents2, PLs),

    % Run timer again
    TRef = gen_fsm:send_event_after(Timeout, update),

    {next_state, active, SD#state{torrents=NewTorrents, timer=TRef}}.


% Another client was connected.
active(add_handler, {Pid, _Tag}, SD=#state{handlers=Hs}) ->
    lager:info("Add the client on ~w.", [Pid]),
    case lists:member(Pid, Hs) of
        true ->
            lager:error("Cannot add a handler twice for the same process ~p.", [Pid]),
            {reply, ok, active, SD};
        false ->
            % Registration of the client
            erlang:monitor(process, Pid),
            SD1 = SD#state{handlers=[Pid|Hs]},
            {reply, ok, active, SD1}
    end;
active(suspend_handler, {Pid, _Tag}, SD=#state{torrents=Torrents}) ->
    case remove_handler(Pid, SD) of
        {next_state, NS, ND} ->
            {reply, Torrents, NS, ND}
    end;
active({resume_handler, OldTorrents}, {Pid, _Tag},
       SD=#state{torrents=NewTorrents, handlers=Hs}) ->
    lager:info("Resume the client from ~w.", [Pid]),
    case lists:member(Pid, Hs) of
        true ->
            lager:error("Cannot add a handler twice for the same process ~p.",
                        [Pid]),
            {reply, ok, active, SD};
        false ->
            % Registration of the client
            erlang:monitor(process, Pid),
            send_diff_records([Pid], OldTorrents, NewTorrents, undefined),
            SD1 = SD#state{handlers=[Pid|Hs]},
            {reply, ok, active, SD1}
    end.


handle_info({'DOWN', _Ref, process, Pid, _Reason}=_Info, active, SD) ->
    remove_handler(Pid, SD).
    

terminate(_Reason, _SN, _SD) ->
    cascadae_event:delete(),
    ok.


code_change(_OldVsn, SN, SD, _Extra) ->
    {ok, SN, SD}.


-spec all_torrents() -> [json_pl()].
all_torrents() ->
    json_torrent_list().


%% @doc Subscribes this process on new messages.
%%      It is used in bullet handler.
%%      You can also run this into your console and use `flush()'.
-spec add_handler() -> SavedState :: term().
add_handler() ->
    gen_fsm:sync_send_event(?SERVER, add_handler).

-spec resume_handler(SavedState :: term()) -> ok.
resume_handler(SavedState) ->
    gen_fsm:sync_send_event(?SERVER, {resume_handler, SavedState}).

suspend_handler() ->
    gen_fsm:sync_send_event(?SERVER, suspend_handler).

%% @doc Sends message from `etorrent_event'. 
%%      This function is called by `cascadae_event'.
fire_event(Mess) ->
    gen_fsm:send_event(?SERVER, {log_event, Mess}).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec json_torrent_list() -> [json_pl()].
json_torrent_list() ->
    List = query_torrent_list(),
    lists:map(form_json_proplist_fn(), List).


-spec query_torrent_list() -> [etorrent_pl()].
query_torrent_list() ->
    etorrent_query:torrent_list().


%% @doc Returns HOF.
%%      The HOF converts a proplist from `etorrent_query:torrent_list'
%%      to a proplist for JSON.
%% @end
form_json_proplist_fn() ->
    fun(X) ->
        %% Data from etorrent_torrent
        Id = proplists:get_value(id, X),
        %% Data from tracking_map (etorrent_table)
        PL = get_torrent(Id),
        Name = proplists:get_value(filename, PL),
        Hash = case proplists:get_value(info_hash, PL) of
                unknown -> <<>>;
                <<InfoHashInt:160>> -> integer_hash_to_literal(InfoHashInt)
            end,

        IsOnline = case proplists:get_value(state, PL) of
                started -> true;
                _ -> false
            end,

        [{id,         Id}
        ,{name,       list_to_binary(Name)}
        ,{display_name, proplists:get_value(display_name, X)}
        ,{total,      proplists:get_value(total, X)}
        ,{wanted,     proplists:get_value(wanted, X)}
        ,{left,       proplists:get_value(left, X)}
        ,{online,     IsOnline}
        ,{leechers,   proplists:get_value(leechers, X)}
        ,{seeders,    proplists:get_value(seeders, X)}
        ,{state,      atom_to_binary(proplists:get_value(state, X))}
        ,{downloaded, proplists:get_value(downloaded, X)}
        ,{uploaded,   proplists:get_value(uploaded, X)}
        ,{all_time_downloaded,  
                        proplists:get_value(all_time_downloaded, X)}
        ,{all_time_uploaded,  
                        proplists:get_value(all_time_uploaded, X)}
        ,{pid,        to_binary(etorrent_torrent_ctl:lookup_server(Id))}
        ,{info_hash,  Hash}
        ]
    end.


-spec get_torrent(torrent_id()) -> etorrent_pl2().
get_torrent(Id) ->
    {value, PL} = etorrent_table:get_torrent(Id),
    PL.

-spec to_record(etorrent_pl()) -> #torrent{}.
to_record(X) ->
    #torrent{
        id       = proplists:get_value(id, X),
        left     = proplists:get_value(left, X),
        wanted   = proplists:get_value(wanted, X),
        leechers = proplists:get_value(leechers, X),
        seeders  = proplists:get_value(seeders, X),
        state    = proplists:get_value(state, X),
        downloaded = proplists:get_value(downloaded, X),
        uploaded   = proplists:get_value(uploaded, X),
        all_time_downloaded = proplists:get_value(all_time_downloaded, X),
        all_time_uploaded   = proplists:get_value(all_time_uploaded, X)
     }.


%% @doc Sort a list by id.
-spec sort_records([#torrent{}]) -> [#torrent{}].
sort_records(List) ->
    lists:keysort(#torrent.id, List).


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
    


-spec diff_records([#torrent{}], [#torrent{}]) -> #diff_acc{}.
diff_records(Olds, News) ->
    diff_records(Olds, News, #diff_acc{}).


%% @doc Calculate the difference beetween two sets of torrents. 
%%
%%      Note: Both lists are sorted.
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

    #torrent{wanted=OW, uploaded=OU, downloaded=OD, state=OS}=Old,
    #torrent{wanted=NW, uploaded=NU, downloaded=ND, state=NS}=New,
    #torrent{all_time_uploaded=OATU, all_time_downloaded=OATD}=Old,
    #torrent{all_time_uploaded=NATU, all_time_downloaded=NATD}=New,
    #torrent{speed_in=OSI, speed_out=OSO}=Old,
    #torrent{speed_in=NSI, speed_out=NSO}=New,

    UnfilteredList = 
    [case NRem of
     ORem -> not_modified;
     X    -> {left, X}
     end
    ,case NW of
     OW   -> not_modified;
     X    -> {wanted, X}
     end
    ,case NLs of
     OLs  -> not_modified;
     X    -> {leechers, X}
     end
    ,case NSs of
     OSs  -> not_modified;
     X    -> {seeders, X}
     end
    ,case ND of
     OD   -> not_modified;
     X    -> {downloaded, X}
     end
    ,case NU of
     OU   -> not_modified;
     X    -> {uploaded, X}
     end
    ,case NATD of
     OATD -> not_modified;
     X    -> {all_time_downloaded, X}
     end
    ,case NATU of
     OATU -> not_modified;
     X    -> {all_time_uploaded, X}
     end
    ,case NS of
     OS   -> not_modified;
     X    -> {state, atom_to_binary(X)}
     end
    ,case NSO of
     OSO  -> not_modified;
     X    -> {speed_out, X} % Byte per second
     end
    ,case NSI of
     OSI  -> not_modified;
     X    -> {speed_in, X}
     end
    ],

    % Filtering and adding id
    case [Y || Y <- UnfilteredList, Y =/=  not_modified] of
    [] -> false;
    FilteredList -> [{id, New#torrent.id} | FilteredList]
    end.


to_binary(Term) ->
    list_to_binary(io_lib:format("~w", [Term])).


atom_to_binary(X) -> list_to_binary(atom_to_list(X)).


%% @doc Search in the list using fun(X) -> boolean().
search([H|T], F) ->
    case F(H) of
    true -> H;
    false -> search(T, F)
    end;
search([], _F) ->
    false.



event_to_json({Name, Id})
    when Name =:= checking_torrent; 
         Name =:= started_torrent;
         Name =:= stopped_torrent ->
    [{name, atom_to_binary(Name)}
    ,{torrent_id, Id}];

event_to_json({Name, Id, Message})
    when Name =:= tracker_error ->
    [{name, atom_to_binary(Name)}
    ,{torrent_id, Id} 
    ,{message, list_to_binary(Message)}];

event_to_json(E) ->
    [{name, <<"unknown">>}
    ,{message, to_binary(E)}].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


sort_records_test_() ->
    Unsorted = [#torrent{id=1}, #torrent{id=3}, #torrent{id=2}],
    Sorted = sort_records(Unsorted),
    [R1, R2, R3] = Sorted,

    [?_assertEqual(R1#torrent.id, 1)
    ,?_assertEqual(R2#torrent.id, 2)
    ,?_assertEqual(R3#torrent.id, 3)
    ].

-endif.


integer_hash_to_literal(InfoHashInt) when is_integer(InfoHashInt) ->
    iolist_to_binary(io_lib:format("~40.16.0B", [InfoHashInt])).


remove_handler(Pid, SD=#state{handlers=Hs, timer=TRef}) ->
    case Hs -- [Pid] of
        [] ->
            cascadae_event:delete(),

            % Go to the sleeping mode.
            gen_fsm:cancel_timer(TRef),
            SD1 = SD#state{handlers=[], torrents=undefined},
            lager:info("Delete the last client on ~w.", [Pid]),
            {next_state, await, SD1};

        NHs ->
            % Just continue.
            lager:info("Delete the client on ~w.", [Pid]),
            {next_state, active, SD#state{handlers=NHs}}
    end.


send_diff_records(Handlers, OldTorrents, NewTorrents, PLs) ->

    #diff_acc{
        diff=Diff, 
        added=Added, 
        deleted=Deleted} = diff_records(OldTorrents, NewTorrents),

    SendFn = fun(Mess) ->
        lists:map(fun(H) ->
            ?HANDLER_MODULE:send(H, Mess)
            end, Handlers)
        end,

    case Diff of
    [] -> skip;
    _  -> 
        SendFn({torrents, {diff_list, Diff}})
    end,

    case Added of
    [] -> skip;
    _  when is_list(PLs) -> 
        Fn = form_json_proplist_fn(),

        % Convert the list of ids to list of JSON.
        AddedJSON = lists:map(fun(Id) ->
                Fn2 = fun(X) -> lists:member({id, Id}, X) end,
                PL = search(PLs, Fn2),
                Fn(PL)
                end, Added),

        SendFn({torrents, {add_list, AddedJSON}});
    _ when PLs =:= undefined ->
        PLs1 = query_torrent_list(),
        Fn = form_json_proplist_fn(),

        % Convert the list of ids to list of JSON.
        AddedJSON = lists:flatmap(fun(Id) ->
                Fn2 = fun(X) -> lists:member({id, Id}, X) end,
                %% If the torrent with ID not found, PL = false,
                %% and result list is empty.
                [Fn(PL) || is_list(PL = search(PLs1, Fn2))]
                end, Added),

        SendFn({torrents, {add_list, AddedJSON}})
    end,

    case Deleted of
    [] -> skip;
    _  -> 
        SendFn({torrents, {delete_list, Deleted}})
    end,

    ok.
