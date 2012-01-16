-module(cascadae_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).
-export([send/2]).

-record(state, {
}).

-define(HUB, cascadae_hub).

init(_Transport, Req, _Opts, _Active) ->
    ok   = ?HUB:add_handler(),
    State = #state{
    },

    {ok, Req, State}.


stream(<<"all_torrents">> = _Data, Req, State) ->
    Data = ?HUB:all_torrents(),
    Respond = [{'event', <<"dataLoadCompleted">>} 
              ,{'data', [{'rows', Data}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

stream(<<"all_peers">> = _Data, Req, State) ->
    case cascadae_peers:all_peers() of
    [] ->
        {ok, Req, State};

    Data ->
        Respond = [{'event', <<"peerDataLoadCompleted">>} 
                  ,{'data', [{'rows', Data}]}
                  ],
        EncodedRespond = jsx:term_to_json(Respond),
        {reply, EncodedRespond, Req, State}
    end;

stream(Data, Req, State) ->
    DecodedData = jsx:json_to_term(Data),
    case proplists:get_value(<<"event">>, DecodedData) of
    <<"remove">> -> 
        Id = proplists:get_value(<<"id">>, DecodedData),
        true = is_number(Id),
        {reply, Data, Req, State};
    <<"pause">> -> 
        Ids = proplists:get_value(<<"ids">>, DecodedData),
        lists:map(fun etorrent_ctl:pause/1, Ids),
        {reply, Data, Req, State};
    <<"continue">> -> 
        Ids = proplists:get_value(<<"ids">>, DecodedData),
        lists:map(fun etorrent_ctl:continue/1, Ids),
        {reply, Data, Req, State};
    _ -> 
        {reply, Data, Req, State}
    end.


info({'diff_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataUpdated">>} 
              ,{'data', [{'rows', Rows}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'add_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataAdded">>} 
              ,{'data', [{'rows', Rows}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'delete_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataRemoved">>} 
              ,{'data', [{'rows', Rows}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'log_event', Mess}=_Info, Req, State) ->
    Respond = [{'event', <<"logEvent">>} 
              ,{'data', Mess}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State}.


terminate(_Req, _State) ->
    ok.


%%
%% API
%%

send(Pid, Mess) ->
    Pid ! Mess.
