-module(cascadae_session).
-export([open/3, recv/4, handle_info/4, close/3]).

-record(session_state, {}).

%% ---- Handlers
open(Pid, Sid, _Opts) ->
    %% Initialization
    error_logger:info_msg("open ~p ~p~n", [Pid, Sid]),
    {ok, #session_state{}}.


recv(_Pid, _Sid, {json, <<>>, Json}, SessionState = #session_state{}) ->
    lager:debug("recv json ~p~n", [Json]),
    {ok, SessionState};

recv(Pid, _Sid, {message, <<>>, Message}, SessionState = #session_state{}) ->
    socketio_session:send_message(Pid, Message),
    {ok, SessionState};

recv(Pid, Sid, Message, SessionState = #session_state{}) ->
    lager:debug("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    {ok, SessionState}.


handle_info(_Pid, _Sid, Info, SessionState = #session_state{}) ->
    lager:debug("Unhandled message recieved ~p.", [Info]),
    {ok, SessionState}.


close(Pid, Sid, _SessionState = #session_state{}) ->
    %% Termination.
    lager:debug("close ~p ~p~n", [Pid, Sid]),
    ok.
