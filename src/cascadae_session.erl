-module(cascadae_session).
-export([send/2]).
-export([open/3, recv/4, handle_info/4, close/3]).

-record(session_state, {
    %% Hash of the cascadae.Table object.
    torrent_table_hash,
    %% Hash of the cascadae.wishlist.List object.
    wish_list_hash,
    %% Id of the ETS table with `#object{}' records.
    obj_tbl,
    %% Pid of `cascadae_files' worker.
    files_pid
}).
-define(OBJ_TBL, cascadae_object_register).
-define(HUB, cascadae_hub).

-record(object, {
    hash,
    path
}).

%% API
send(Pid, Mess) ->
    Pid ! Mess.

%% ---- Handlers
open(Pid, Sid, _Opts) ->
    %% Initialization
    ?HUB:add_handler(),
    error_logger:info_msg("open ~p ~p~n", [Pid, Sid]),
    ObjTbl = ets:new(?OBJ_TBL, [{keypos, #object.hash}]),
    {ok, #session_state{obj_tbl=ObjTbl}}.


recv(_Pid, _Sid, {json, <<>>, Json}, State = #session_state{}) ->
    lager:debug("recv json ~p~n", [Json]),
    {ok, State};

recv(_Pid, _Sid, {message, <<>>, _Message}, State = #session_state{}) ->
%%  socketio_session:send_message(Pid, Message),
    {ok, State};

recv(_, _, {event, <<>>, <<"deactivated">>, Meta}=Mess, State) ->
    #session_state{files_pid=FilesPid} = State,
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_hash(Hash),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
         [<<"cascadae">>,<<"files">>,<<"Tree">>] when is_pid(FilesPid) ->
            cascadae_files:deactivate(FilesPid),
            {ok, State};
         _ ->
            lager:debug("Ignore ~p.", [Mess]),
            {ok, State}
    end;

recv(_, _, {event, <<>>, <<"activated">>, Meta}=Mess, State) ->
    #session_state{files_pid=FilesPid} = State,
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_hash(Hash),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
         [<<"cascadae">>,<<"files">>,<<"Tree">>] when is_pid(FilesPid) ->
            cascadae_files:activate(FilesPid),
            {ok, State};
         _ ->
            lager:debug("Ignore ~p.", [Mess]),
            {ok, State}
    end;

recv(_, _, {event, <<>>, <<"registerObject">>, Meta}, State) ->
    Path = proplists:get_value(<<"path">>, Meta),
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_path(Path),
    assert_hash(Hash),
    {ok, register_object(Path, Hash, State)};

%% TORRENTS
recv(_, _, {event, <<>>, <<"d_startTorrents">>, Meta}, State) ->
    Data = proplists:get_value(<<"data">>, Meta),
    Ids = proplists:get_value(<<"torrent_ids">>, Data),
    lists:map(fun etorrent_ctl:continue/1, Ids),
    {ok, State};
recv(_, _, {event, <<>>, <<"d_stopTorrents">>, Meta}, State) ->
    Data = proplists:get_value(<<"data">>, Meta),
    Ids = proplists:get_value(<<"torrent_ids">>, Data),
    lists:map(fun etorrent_ctl:pause/1, Ids),
    {ok, State};

%% FILES
%% Request children of the file tree.
recv(Session, _, {event, <<>>, <<"d_childrenRequest">>, Meta}, State) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    TorrentID = proplists:get_value(<<"torrent_id">>, Data),
    FileIDs   = proplists:get_value(<<"file_ids">>, Data),
    assert_hash(Hash),
    FilesPid = %% try start
        case State#session_state.files_pid of
            undefined ->
                {ok, Pid} = cascadae_files:start_link(Session, {files, Hash}),
                Pid;
            Pid -> Pid
        end,
    cascadae_files:request(FilesPid, TorrentID, FileIDs),
    {ok, State#session_state{files_pid=FilesPid}};
recv(Session, _, {event, <<>>, <<"d_wishFiles">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        FileIDs   = proplists:get_value(<<"file_ids">>, Data),
        {ok, NewWishes} = etorrent_torrent_ctl:wish_file(TorrentID, FileIDs),
        case State#session_state.wish_list_hash of
            undefined -> ok;
            WLHash ->
                RespondData = encode_wishes(TorrentID, NewWishes), 
                fire_data_event(Session, WLHash, <<"rd_wishesRespond">>,
                                RespondData) 
        end
        end),
    {ok, State};
recv(_, _, {event, <<>>, <<"d_skipFiles">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        FileIDs   = proplists:get_value(<<"file_ids">>, Data),
        ok        = etorrent_torrent_ctl:skip_file(TorrentID, FileIDs)
        end),
    {ok, State};
recv(_, _, {event, <<>>, <<"d_unskipFiles">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        FileIDs   = proplists:get_value(<<"file_ids">>, Data),
        ok        = etorrent_torrent_ctl:unskip_file(TorrentID, FileIDs)
        end),
    {ok, State};

%% WISHES
recv(Session, _, {event, <<>>, <<"d_wishesRequest">>, Meta}, State) ->
    spawn_link(fun() ->
        Hash      = proplists:get_value(<<"hash">>, Meta),
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        {ok, Wishes} = etorrent_torrent_ctl:get_wishes(TorrentID),
        RespondData = encode_wishes(TorrentID, Wishes), 
        fire_data_event(Session, Hash, <<"rd_wishesRespond">>, RespondData) 
        end),
    {ok, State};
recv(_, _, {event, <<>>, <<"d_wishesSave">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        NewWishes = proplists:get_value(<<"new_wishes">>, Data),
        Wishes = decode_wishes(NewWishes),
        lager:info("Set new wishes ~p for torrent ~B.", [Wishes, TorrentID]),
        {ok, _Wishes1} = etorrent_torrent_ctl:set_wishes(TorrentID, Wishes)
        end),
    {ok, State};

recv(Pid, Sid, Message, State) ->
    lager:debug("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    {ok, State}.


handle_info(Pid, _, {torrents, What},
            State=#session_state{torrent_table_hash=Hash})
    when is_binary(Hash) ->
    case What of
        {diff_list, Rows} ->
            fire_data_event(Pid, Hash, <<"rd_dataUpdated">>,
                            [{<<"rows">>, Rows}]);
        {add_list, Rows} ->
            fire_data_event(Pid, Hash, <<"rd_dataAdded">>,
                            [{<<"rows">>, Rows}]);
        {delete_list, Rows} ->
            fire_data_event(Pid, Hash, <<"rd_dataRemoved">>,
                            [{<<"rows">>, Rows}])
    end,
    {ok, State};
handle_info(Pid, _, {{files, Hash}, What}, State=#session_state{}) ->
    case What of
        {add_list, TorrentID, Nodes} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Nodes}],
            fire_data_event(Pid, Hash, <<"rd_childrenRespond">>, Data);
        {diff_list, TorrentID, Diff} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Diff}],
            fire_data_event(Pid, Hash, <<"rd_dataUpdated">>, Data)
    end,
    {ok, State};
handle_info(Pid, _, {log_event, Mess}, State = #session_state{}) ->
    Message = {event, <<"rd_logEvent">>, Mess},
    socketio_session:send(Pid, Message),
    {ok, State};

handle_info(_Pid, _Sid, Info, State = #session_state{}) ->
    lager:debug("Unhandled message recieved ~p.", [Info]),
    {ok, State}.


close(Pid, Sid, #session_state{}) ->
    %% Termination.
    lager:debug("close ~p ~p~n", [Pid, Sid]),
    ok.


%% ------------------------------------------------------------------

register_object(Path=[<<"cascadae">>,<<"Table">>], Hash, State) ->
    Session = self(),
    proc_lib:spawn_link(fun() ->
            Torrents = ?HUB:all_torrents(),
            fire_data_event(Session, Hash, <<"rd_dataLoadCompleted">>,
                            [{<<"rows">>, Torrents}])
        end),
    add_event_listener(Session, Hash, <<"d_startTorrents">>),
    add_event_listener(Session, Hash, <<"d_stopTorrents">>),
    store_object(Path, Hash, State#session_state{torrent_table_hash=Hash});
register_object(Path=[<<"cascadae">>,<<"wishlist">>,<<"List">>], Hash, State) ->
    Session = self(),
    add_event_listener(Session, Hash, <<"d_wishesSave">>),
    add_event_listener(Session, Hash, <<"d_wishesRequest">>),
    store_object(Path, Hash, State#session_state{wish_list_hash=Hash});
register_object(Path=[<<"cascadae">>,<<"files">>,<<"Tree">>], Hash, State) ->
    Session = self(),
    add_event_listener(Session, Hash, <<"d_childrenRequest">>),
    add_event_listener(Session, Hash, <<"d_wishFiles">>),
    add_event_listener(Session, Hash, <<"d_skipFiles">>),
    add_event_listener(Session, Hash, <<"d_unskipFiles">>),
    add_event_listener(Session, Hash, <<"activated">>),
    add_event_listener(Session, Hash, <<"deactivated">>),
    store_object(Path, Hash, State);
register_object(Path=[<<"cascadae">>,<<"peers">>,<<"Table">>], Hash, State) ->
%   Session = self(),
%   proc_lib:spawn_link(fun() ->
%           Peers = cascadae_peers:all_peers(),
%           fire_data_event(Session, Hash, <<"rd_dataLoadCompleted">>,
%                           [{<<"rows">>, Peers}])
%       end),
    store_object(Path, Hash, State);
register_object(Path, Hash, State) ->
    lager:warning("Cannot register objest ~p:~p.", [Path, Hash]),
    State.


store_object(Path, Hash, State=#session_state{obj_tbl=ObjTbl}) ->
    ets:insert_new(ObjTbl, #object{path=Path, hash=Hash}),
    State.

extract_object(Hash, #session_state{obj_tbl=ObjTbl}) ->
    case ets:lookup(ObjTbl, Hash) of
        [X] -> X
    end.


fire_data_event(Session, Hash, EventName, EventData) ->
    lager:debug("Fire data event ~p with ~p.", [EventName, EventData]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash},
            {<<"data">>, EventData}],
    Message = {event, <<"fireDataEvent">>, Args},
    socketio_session:send(Session, Message).


add_event_listener(Session, Hash, EventName) ->
    lager:debug("Add event listener ~p.", [EventName]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash}],
    Message = {event, <<"addListener">>, Args},
    socketio_session:send(Session, Message).



assert_path(Path) when is_list(Path) -> ok.
assert_hash(Hash) when is_binary(Hash) -> ok.



encode_wishes(TorrentId, Wishes) ->
    XX   = [encode_wish(TorrentId, X) || X <- Wishes],
    List = [Y || Y <- XX, Y =/= false],
   [{torrent_id, TorrentId} , {list, List}].


encode_wish(TorrentId, X) ->
    V = proplists:get_value(value, X),
    C = proplists:get_value(is_completed, X),
    T = proplists:get_value(is_transient, X),

    case proplists:get_value(type, X) of
    file ->
          [ {name, etorrent_info:long_file_name(TorrentId, V)}
          , {value, V}
          , {is_completed, C}
          , {is_transient, T}
          , {type, <<"file">>}
          ];

    piece ->
          Name = iolist_to_binary(io_lib:format("~w", [V])),
          [ {name, Name}
          , {value, V}
          , {is_completed, C}
          , {is_transient, T}
          , {type, <<"piece">>}
          ];

    _ -> false
    end.

decode_wishes(Wishes) ->
    [[{ binary_to_existing_atom(K, utf8),
        if is_binary(V) -> binary_to_existing_atom(V, utf8);
                   true -> V
        end} || {K, V} <- X, K =/= <<"name">>] || X <- Wishes].
