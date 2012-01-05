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



-record(state, {
    config :: [{atom(), term()}],
    remote_node :: atom()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Args = [{'node', 'etorrent@127.0.0.1'}
           ,{'cookie', 'etorrent'}
           ],
    gen_server:start_link(?MODULE, Args, []).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Config) ->
    Node = proplists:get_value('node', Config),

    % Connect to the etorrent node
    case proplists:get_value('cookie', Config) of
    'undefined' ->
        ok;
    Cookie ->
        erlang:set_cookie(Node, Cookie)
    end,
    true = net_kernel:connect_node(Node),

    State = #state{
        config = Config,
        remote_node = Node
    },
    {ok, State}.


handle_call('get_entire_torrent_list', _From, State) ->
    Reply = torrent_list_rpc(State#state.remote_node),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_entire_torrent_list(Server) ->
    gen_server:call(Server, 'get_entire_torrent_list').

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

torrent_list_rpc(RemoteNode) ->
    List = rpc:call(RemoteNode, etorrent_query, torrent_list, []),
    lists:map(fun(X) ->
       [{'id', proplists:get_value('id', X)}
       ,{'total', proplists:get_value('total', X)}
       ,{'online', 'false'}
       ]
    end, List).


