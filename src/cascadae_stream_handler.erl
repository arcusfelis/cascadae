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
    Data = cascadae_peers:all_peers(),
    Respond = [{'event', <<"peerDataLoadCompleted">>} 
              ,{'data', [{'rows', Data}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

stream(Data, Req, State) ->
    DecodedData = jsx:json_to_term(Data),
    lager:debug("Event: ~p", [DecodedData]),
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

    <<"file_list">> ->
        %% Load file list
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        Parents   = proplists:get_value(<<"parent_ids">>, DecodedData),

        Nodes = lists:map(fun(ParentId) ->
                Children = etorrent_info:tree_children(TorrentId, ParentId),
                [ {'parent_id', ParentId}
                , {'children', Children}
                ]
            end, Parents),
        
        Respond = [ {'event', <<"fileDataLoadCompleted">>} 
                  , {'data', [ {'torrent_id', TorrentId}
                             , {'nodes', Nodes}]}
                  ],
        EncodedRespond = jsx:term_to_json(Respond),
        {reply, EncodedRespond, Req, State};

    <<"wish_list">> ->
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        {ok, Wishes} = etorrent_torrent_ctl:get_wishes(TorrentId),

        EncodedRespond = encode_wishes(TorrentId, Wishes),
        {reply, EncodedRespond, Req, State};

    <<"replace_wish_list">> ->
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        List = proplists:get_value(<<"list">>, DecodedData),
        Wishes = [[{ binary_to_existing_atom(K)
                   , if
                        is_binary(V) -> binary_to_existing_atom(V);
                        true -> V
                     end} 
                    || {K, V} <- X, K =/= <<"name">>] || X <- List],
        error_logger:info_msg("Set new wishes ~p for torrent ~B.",
            [Wishes, TorrentId]),
        {ok, _Wishes1} = etorrent_torrent_ctl:set_wishes(TorrentId, Wishes),
        {ok, Req, State};

    <<"wish_files">> ->
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        Fids      = proplists:get_value(<<"file_ids">>, DecodedData),
        {ok, NewWishes} = etorrent_torrent_ctl:wish_file(TorrentId, Fids),
        EncodedRespond = encode_wishes(TorrentId, NewWishes),
        {reply, EncodedRespond, Req, State};

    <<"skip_files">> ->
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        Fids      = proplists:get_value(<<"file_ids">>, DecodedData),
        ok        = etorrent_torrent_ctl:skip_file(TorrentId, Fids),
        {ok, Req, State};

    <<"unskip_files">> ->
        TorrentId = proplists:get_value(<<"torrent_id">>, DecodedData),
        Fids      = proplists:get_value(<<"file_ids">>, DecodedData),
        ok        = etorrent_torrent_ctl:unskip_file(TorrentId, Fids),
        {ok, Req, State};

    _ -> 
        {reply, Data, Req, State}
    end.


encode_wishes(TorrentId, Wishes) ->
    XX   = [encode_wish(TorrentId, X) || X <- Wishes],
    List = [Y || Y <- XX, Y =/= false],

    Respond = [ {'event', <<"wishDataLoadCompleted">>} 
              , {'data', [ {'torrent_id', TorrentId}
                         , {'list', List}]}
              ],
    jsx:term_to_json(Respond).


encode_wish(TorrentId, X) ->
    V = proplists:get_value('value', X),
    C = proplists:get_value('is_completed', X),
    T = proplists:get_value('is_transient', X),

    case proplists:get_value('type', X) of
    'file' ->
          [ {'name', etorrent_info:long_file_name(TorrentId, V)}
          , {'value', V}
          , {'is_completed', C}
          , {'is_transient', T}
          , {'type', <<"file">>}
          ];

    'piece' ->
          Name = unicode:characters_to_binary(io_lib:format("~w", [V])),
          [ {'name', Name}
          , {'value', V}
          , {'is_completed', C}
          , {'is_transient', T}
          , {'type', <<"piece">>}
          ];

    _ -> false
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



binary_to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).


atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).
