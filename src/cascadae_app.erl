-module(cascadae_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).
-define(APP, cascadae).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(mimetypes),
	application:start(cowboy),
	application:start(jsx),
	application:start(socketio),
	application:start(cascadae).


start(_Type, Args) ->
    Envs = application:get_all_env(cascadae),
    lager:info("Starting application \"cascadae\" with arguments ~p. "
                "Environment variables are ~p.", [Args, Envs]),
    IsEnabled  = proplists:get_value(webui, Envs),
    ListenPort = proplists:get_value(webui_port, Envs),
    %% Errors :: {Reason :: term(), Failed :: boolean()}
    Errors = [{disabled, IsEnabled /= true},
              {{required_param, webui_port}, not is_integer(ListenPort)}],
    ErrorReasons = [Reason || {Reason, true} <- Errors],
    case ErrorReasons of
        []    -> start_webui(ListenPort);
        [_|_] -> {error, ErrorReasons}
    end.


start_webui(ListenPort) ->
    PrivDir     = code:priv_dir(?APP),
    BuildDir    = abs_path(filename:join([PrivDir, "html"])),
    SIODir      = abs_path(filename:join([PrivDir, "socket.io-client"])),
    QDepsDir    = abs_path(code:lib_dir(?APP, q_deps)),
    QSrcDir     = abs_path(code:lib_dir(?APP, q_src)),
    StaticFilesCfg = [{mimetypes, {fun mimetypes:path_to_mimes/2, default}},
                      {etag, {attributes, [filepath, filesize, inode, mtime]}}],
    SIOConfig = socketio_session:configure([
            {heartbeat, 5000},
%           {session_timeout, 30000},
            {callback, cascadae_session},
            {protocol, socketio_data_protocol}]),

    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cascadae_default_handler, []},

            {"/socket.io/1/[...]", socketio_handler, [SIOConfig]},

            {"/q_deps/[...]", cowboy_static,
                 [{directory, QDepsDir}|StaticFilesCfg]},

            {"/q_src/[...]", cowboy_static,
                 [{directory, QSrcDir}|StaticFilesCfg]},

            {"/priv/html/[...]", cowboy_static,
                 [{directory, BuildDir}|StaticFilesCfg]},

            {"/socket.io-client/[...]", cowboy_static,
                 [{directory, SIODir}|StaticFilesCfg]},

            {"/[...]", cowboy_static,
                 [{directory, BuildDir}|StaticFilesCfg]}
		]}
    ]),
    {ok, _} = cowboy:start_http(cascadae_http, 100, [{port, ListenPort}], [
                    {env, [{dispatch, Dispatch}]}
                ]),
	cascadae_sup:start_link().

stop(_State) ->
	ok.


%%
%% Private
%%

abs_path(Path) -> 
    filename:join(
        abs_path_(
            filename:split(
                filename:absname(Path)), [])).

abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).
