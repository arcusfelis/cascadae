-module(cascadae_speed_hub).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([add_handler/0,
        resume_handler/1,
        suspend_handler/0]).

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


-record(state, {
    timer :: reference(),
    speed :: {non_neg_integer(), non_neg_integer()} | undefined,
    handlers = [] :: [pid()],
    tick :: integer()
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Args = [1000],
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


await(_, SD) ->
    {next_state, await, SD}.


% Initialization after "sleeping mode".
await(add_handler, {Pid, _Tag}, SD=#state{tick=Timeout}) ->
    lager:info("Add the first client on ~w.", [Pid]),

    % Registration of the client
    erlang:monitor(process, Pid),
    
    NewSpeed = speed_rate(),
    send_init([Pid], NewSpeed),

    % Run timer
    TRef = gen_fsm:send_event_after(Timeout, update),
    SD1 = SD#state{timer=TRef, 
                handlers=[Pid],
                speed=NewSpeed},

    {reply, NewSpeed, active, SD1};
await({resume_handler, OldSpeed},
      {Pid, _Tag}, SD=#state{tick=Timeout}) ->
    lager:info("Add the first client on ~w.", [Pid]),

    % Registration of the client
    erlang:monitor(process, Pid),
    
    NewSpeed = speed_rate(),

    % Collect data
    send_diff([Pid], OldSpeed, NewSpeed),

    % Run timer
    TRef = gen_fsm:send_event_after(Timeout, update),
    SD1 = SD#state{timer=TRef, 
                handlers=[Pid],
                speed=NewSpeed},

    {reply, ok, active, SD1}.


% It is called because gen_fsm:send_event_after.
active(update, SD=#state{
        speed=OldSpeed, 
        handlers=Handlers,
        tick=Timeout}) ->
    NewSpeed = speed_rate(),

    % proplists from etorrent.
    send_diff(Handlers, OldSpeed, NewSpeed),

    % Run timer again
    TRef = gen_fsm:send_event_after(Timeout, update),

    {next_state, active, SD#state{speed=NewSpeed, timer=TRef}}.


% Another client was connected.
active(add_handler, {Pid, _Tag}, SD=#state{handlers=Hs, speed=OldSpeed}) ->
    lager:info("Add the client on ~w.", [Pid]),
    case lists:member(Pid, Hs) of
        true ->
            lager:error("Cannot add a handler twice for the same process ~p.", [Pid]),
            {reply, ok, active, SD};
        false ->
            % Registration of the client
            erlang:monitor(process, Pid),
            send_init([Pid], OldSpeed),
            SD1 = SD#state{handlers=[Pid|Hs]},
            {reply, ok, active, SD1}
    end;
active(suspend_handler, {Pid, _Tag}, SD=#state{speed=OldSpeed}) ->
    case remove_handler(Pid, SD) of
        {next_state, NS, ND} ->
            {reply, OldSpeed, NS, ND}
    end;
active({resume_handler, OldSpeed}, {Pid, _Tag},
       SD=#state{speed=NewSpeed, handlers=Hs}) ->
    lager:info("Resume the client from ~w.", [Pid]),
    case lists:member(Pid, Hs) of
        true ->
            lager:error("Cannot add a handler twice for the same process ~p.",
                        [Pid]),
            {reply, ok, active, SD};
        false ->
            % Registration of the client
            erlang:monitor(process, Pid),
            send_diff([Pid], OldSpeed, NewSpeed),
            SD1 = SD#state{handlers=[Pid|Hs]},
            {reply, ok, active, SD1}
    end.


handle_info({'DOWN', _Ref, process, Pid, _Reason}=_Info, active, SD) ->
    remove_handler(Pid, SD).
    

terminate(_Reason, _SN, _SD) ->
    ok.


code_change(_OldVsn, SN, SD, _Extra) ->
    {ok, SN, SD}.


%% @doc Subscribes this process on new messages.
%%      It is used in bullet handler.
%%      You can also run this into your console and use `flush().
-spec add_handler() -> SavedState :: term().
add_handler() ->
    gen_fsm:sync_send_event(?SERVER, add_handler).

-spec resume_handler(SavedState :: term()) -> ok.
resume_handler(SavedState) ->
    gen_fsm:sync_send_event(?SERVER, {resume_handler, SavedState}).

suspend_handler() ->
    gen_fsm:sync_send_event(?SERVER, suspend_handler).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


remove_handler(Pid, SD=#state{handlers=Hs, timer=TRef}) ->
    case Hs -- [Pid] of
        [] ->
            % Go to the sleeping mode.
            gen_fsm:cancel_timer(TRef),
            SD1 = SD#state{handlers=[], speed=undefined},
            lager:info("Delete the last client on ~w.", [Pid]),
            {next_state, await, SD1};

        NHs ->
            % Just continue.
            lager:info("Delete the client on ~w.", [Pid]),
            {next_state, active, SD#state{handlers=NHs}}
    end.


send_diff(_Handlers, OldSpeed, OldSpeed) ->
    ok;
send_diff(Handlers, {OldSend, OldRecv, OldMaxSend, OldMaxRecv},
                    {NewSend, NewRecv, NewMaxSend, NewMaxRecv}) ->
    M = {speed_rate_update,
         [{send_rate, NewSend} || NewSend =/= OldSend] ++
         [{recv_rate, NewRecv} || NewRecv =/= OldRecv] ++
         [{max_send_rate, NewMaxSend} || NewMaxSend =/= OldMaxSend] ++
         [{max_recv_rate, NewMaxRecv} || NewMaxRecv =/= OldMaxRecv]},
    lists:foldl(fun ?HANDLER_MODULE:send/2, M, Handlers).

send_init(Handlers, {NewSend, NewRecv, NewMaxSend, NewMaxRecv}) ->
    M = {speed_rate_update,
         [{send_rate, NewSend}
         ,{recv_rate, NewRecv}
         ,{max_send_rate, NewMaxSend}
         ,{max_recv_rate, NewMaxRecv}]},
    lists:foldl(fun ?HANDLER_MODULE:send/2, M, Handlers).


speed_rate() ->
    {etorrent_rlimit:send_rate(), etorrent_rlimit:recv_rate(),
     etorrent_rlimit:max_send_rate(), etorrent_rlimit:max_recv_rate()
    }.

