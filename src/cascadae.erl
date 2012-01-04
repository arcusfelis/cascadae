%% Feel free to use, reuse and abuse the code in this file.

-module(cascadae).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(cascadae).


start(_Type, _Args) ->
    PrivDir = code:priv_dir(cascadae),
    BuildDir = abs_path(filename:join(
            [PrivDir, "rhyacotriton"])),
    JQueryDir = abs_path(filename:join(
            [PrivDir, "jquery"])),
    BulletDir = abs_path(code:priv_dir(bullet)),

	Dispatch = [
		{'_', [
            cowboy_static:rule([
                {dir, BuildDir}, 
                {prefix, ""}, 
                {sendfile, false}]),
            cowboy_static:rule([
                {dir, JQueryDir}, 
                {prefix, "jquery"}, 
                {sendfile, false}]),
            cowboy_static:rule([
                {dir, BulletDir}, 
                {prefix, "bullet"}, 
                {sendfile, false}]),
			{[<<"stream">>], bullet_handler, [{handler, c_stream_handler}]}
		]}
	],
	cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	cowboy:start_listener(https, 100,
		cowboy_ssl_transport, [
			{port, 8443}, {certfile, "priv/ssl/cert.pem"},
			{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
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
