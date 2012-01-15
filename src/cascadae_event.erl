%%% @doc This module is a Erlang event handler.
%%%      It sends events to the process using message passing.

-module(cascadae_event).
-behaviour(gen_event).
-export([add/0]).
-export([init/1, 
        handle_event/2, 
        terminate/2]).


add() ->
    etorrent_event:add_handler(?MODULE, []).

% ---------------------------------------------------------

init([]) ->
    {ok, []}.

handle_event(Mess, S) ->
    cascadae_hub:fire_event(Mess),
    {ok, S}.

terminate(_Args, Fd) ->
    file:close(Fd).
