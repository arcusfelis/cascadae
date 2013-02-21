%% Feel free to use, reuse and abuse the code in this file.

-module(cascadae).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(mimetypes),
	application:start(cowboy),
	application:start(jsx),
	application:start(bullet),
	application:start(cascadae).


start(_Type, _Args) ->
    PrivDir = code:priv_dir(cascadae),
    BuildDir = abs_path(filename:join(
            [PrivDir, "rhyacotriton"])),
    JQueryDir = abs_path(filename:join(
            [PrivDir, "jquery"])),
    BulletDir = abs_path(code:priv_dir(bullet)),
    StaticFilesCfg = [{mimetypes, {fun mimetypes:path_to_mimes/2, default}}],

    Dispatch = cowboy_router:compile([
		{'_', [
			{"/stream", bullet_handler, 
                    [{handler, cascadae_stream_handler}]},

			{"/", cascadae_default_handler, []},

            {"/jquery/[...]", cowboy_static,
                 [{directory, JQueryDir}|StaticFilesCfg]},

            {"/bullet/[...]", cowboy_static,
                 [{directory, BulletDir}|StaticFilesCfg]},

            {"/[...]", cowboy_static,
                 [{directory, BuildDir}|StaticFilesCfg]}
		]}
    ]),
    {ok, _} = cowboy:start_http(cascadae_http, 100, [{port, 1080}], [
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
