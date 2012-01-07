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


-record(torrent, {
    'id',
    'left',
    'leechers',
    'seeders'
}).


-record(state, {
    config :: [{atom(), term()}],
    remote_node :: atom(),
    timer :: reference(),
    torrents :: [#torrent{}],
    handler :: pid()
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Handler = self(),
    Config = [{'node', 'etorrent@127.0.0.1'}
             ,{'cookie', 'etorrent'}
             ],
    Args = [Handler, Config],
    gen_server:start_link(?MODULE, Args, []).



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Handler, Config]) ->
    RemoteNode = proplists:get_value('node', Config),

    % Connect to the etorrent node
    case proplists:get_value('cookie', Config) of
    'undefined' ->
        ok;
    Cookie ->
        erlang:set_cookie(RemoteNode, Cookie)
    end,
    true = net_kernel:connect_node(RemoteNode),

    {ok, TRef} = timer:send_interval(1000, 'tick'),
    NewTorrents = torrent_list_of_records_rpc(RemoteNode),
    State = #state{
        config = Config,
        remote_node = RemoteNode,
        timer = TRef,
        handler = Handler,
        torrents = NewTorrents
    },

    {ok, State}.


handle_call('get_entire_torrent_list', _From, State) ->
    Reply = torrent_list_rpc(State#state.remote_node),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info('tick'=_Info, State=#state{
        torrents=OldTorrents, 
        remote_node=RemoteNode,
        handler=Handler}) ->
    NewTorrents = torrent_list_of_records_rpc(RemoteNode),
    case diff_list(OldTorrents, NewTorrents) of
    [] -> 'skip';
    Diff -> 
        % TODO: fix the direct message passing
        Handler ! {'diff_list', Diff}
    end,
    {noreply, State#state{torrents=NewTorrents}}.

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
        Id = proplists:get_value('id', X),
        PL = torrent_rpc(RemoteNode, Id),
        Name = proplists:get_value('filename', PL),

        [{'id',       Id}
        ,{'name',     list_to_binary(Name)}
        ,{'total',    proplists:get_value('total', X)}
        ,{'left',     proplists:get_value('left', X)}
        ,{'online',  'false'}
        ,{'leechers', proplists:get_value('leechers', X)}
        ,{'seeders',  proplists:get_value('seeders', X)}
        ]
    end, List).

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

torrent_rpc(RemoteNode, Id) ->
    {value, PL} = rpc:call(RemoteNode, etorrent_table, get_torrent, [Id]),
    PL.


torrent_list_of_records_rpc(RemoteNode) ->
    List = rpc:call(RemoteNode, etorrent_query, torrent_list, []),
    [#torrent{
        id       = proplists:get_value('id', X),
        left     = proplists:get_value('left', X),
        leechers = proplists:get_value('leechers', X),
        seeders  =  proplists:get_value('seeders', X)
     } || X <- List].


-spec diff_list([#torrent{}], [#torrent{}]) -> [[{atom(), term()}]].
diff_list(OldList, NewList) ->
    ResultList = lists:map(
        fun(New) ->
            % Old and New have same id.
            case lists:keysearch(New#torrent.id, #torrent.id, OldList) of
            false -> false; % added torrent
            {value, Old} -> diff_element(Old, New)
            end
        end,  NewList),
    [X || X <- ResultList, X =/= false].


-spec diff_element(#torrent{}, #torrent{}) -> [{atom(), term()}].
diff_element(_Old=#torrent{left=ORem, leechers=OLs, seeders=OSs},
              New=#torrent{left=NRem, leechers=NLs, seeders=NSs}) ->
    UnfilteredList = 
    [case ORem of
     NRem -> 'not_modified';
     X    -> {'left', X}
     end
    ,case OLs of
     NLs -> 'not_modified';
     X   -> {'leechers', X}
     end
    ,case OSs of
     NSs -> 'not_modified';
     X   -> {'seeders', X}
     end
    ],
    % Filtering and adding id
    case [Y || Y <- UnfilteredList, Y =/=  'not_modified'] of
    [] -> false;
    FilteredList -> [{'id', New#torrent.id} | FilteredList]
    end.


