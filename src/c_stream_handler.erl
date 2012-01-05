-module(c_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {
    rpc_server
}).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Pid} = c_etorrent_rpc:start_link(),
    State = #state{
        rpc_server = Pid
    },

    {ok, Req, State}.

stream(<<"get_entire_torrent_list">> = _Data, Req, State) ->
    Node = State#state.rpc_server,
    Data = c_etorrent_rpc:get_entire_torrent_list(Node),
    Respond = [{'event', <<"dataLoadCompleted">>} 
              ,{'data', [{'rows', Data}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};
stream(Data, Req, State) ->
    {reply, Data, Req, State}.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.
