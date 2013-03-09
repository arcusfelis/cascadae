-module(cascadae_session).
-export([open/3, recv/4, handle_info/4, close/3]).

-record(session_state, {}).
-define(OBJ_TBL, cascadae_object_register).

-record(object, {
    hash,
    path
}).

%% ---- Handlers
open(Pid, Sid, _Opts) ->
    %% Initialization
    error_logger:info_msg("open ~p ~p~n", [Pid, Sid]),
    ets:new(?OBJ_TBL, [named_table, {keypos, #object.hash}]),
    {ok, #session_state{}}.


recv(_Pid, _Sid, {json, <<>>, Json}, State = #session_state{}) ->
    lager:debug("recv json ~p~n", [Json]),
    {ok, State};

recv(_Pid, _Sid, {message, <<>>, _Message}, State = #session_state{}) ->
%%  socketio_session:send_message(Pid, Message),
    {ok, State};

recv(_Pid, _Sid, {event, <<>>, <<"registerObject">>, Data}, State = #session_state{}) ->
    Path = proplists:get_value(<<"path">>, Data),
    Hash = proplists:get_value(<<"hash">>, Data),
    {ok, register_object(Path, Hash, State)};
recv(Pid, Sid, Message, State = #session_state{}) ->
    lager:debug("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    {ok, State}.


handle_info(_Pid, _Sid, Info, State = #session_state{}) ->
    lager:debug("Unhandled message recieved ~p.", [Info]),
    {ok, State}.


close(Pid, Sid, _State = #session_state{}) ->
    %% Termination.
    lager:debug("close ~p ~p~n", [Pid, Sid]),
    ok.


%% ------------------------------------------------------------------

register_object(Path=[<<"cascadae">>,<<"Table">>], Hash, State) ->
    store_object(Path, Hash, State);
register_object(Path, Hash, State) ->
    lager:warning("Cannot register objest ~p:~p.", [Path, Hash]),
    State.


store_object(Path, Hash, State) ->
    ets:insert_new(?OBJ_TBL, #objest{path=Path, hash=Hash}),
    State.

extract_object(Hash, _State) ->
    ets:lookup_element(?OBJ_TBL, Hash).
