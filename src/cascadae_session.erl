-module(cascadae_session).
-export([send/2]).
-export([open/3, recv/4, handle_info/4, close/3]).

-record(sess_state, {
    %% Hash of the cascadae.Table object.
    torrent_table_hash,
    %% Hash of the cascadae.wishlist.List object.
    wish_list_hash,
    %% Id of the ETS table with `#object{}' records.
    obj_tbl,
    %% Pid of `cascadae_files' worker.
    files_pid,
    %% Is an interval timer for updates active?
    is_files_active = true,
    is_files_visible,
    peers_pid,
    is_peers_active = true,
    is_peers_visible,
    is_page_visible
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
    {ok, #sess_state{obj_tbl=ObjTbl}}.


%% Pid is not self().
recv(_Pid, _Sid, {json, <<>>, Json}, State = #sess_state{}) ->
    lager:debug("recv json ~p~n", [Json]),
    {ok, State};

recv(_Pid, _Sid, {message, <<>>, _Message}, State = #sess_state{}) ->
%%  socketio_session:send_message(Pid, Message),
    {ok, State};

recv(_, _, {event, <<>>, <<"deactivated">>, Meta}=Mess, State) ->
    #sess_state{files_pid=FilesPid, peers_pid=PeersPid} = State,
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_hash(Hash),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
         [<<"cascadae">>,<<"files">>,<<"Tree">>] when is_pid(FilesPid) ->
            {ok, check_file_visibility(State#sess_state{is_files_visible=false})};
         [<<"cascadae">>,<<"peers">>,<<"Table">>]  when is_pid(PeersPid) ->
            {ok, check_peer_visibility(State#sess_state{is_peers_visible=false})};
         [<<"cascadae">>,<<"Container">>] ->
            {ok, check_peer_visibility(
                 check_file_visibility(State#sess_state{is_page_visible=false}))};
         _ ->
            lager:debug("Ignore ~p.", [Mess]),
            {ok, State}
    end;

recv(_, _, {event, <<>>, <<"activated">>, Meta}=Mess, State) ->
    #sess_state{files_pid=FilesPid} = State,
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_hash(Hash),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
         [<<"cascadae">>,<<"files">>,<<"Tree">>] when is_pid(FilesPid) ->
            %% It is only meaningful, if the process started.
            {ok, check_file_visibility(State#sess_state{is_files_visible=true})};
         [<<"cascadae">>,<<"peers">>,<<"Table">>] ->
            %% It is only meaningful, if nothing is selected.
            %% Start here.
            PeersPid = %% try start
                case State#sess_state.peers_pid of
                    undefined ->
                        {ok, Pid} = cascadae_peers:start_link(self(),
                                                              {peers, Hash}),
                        Pid;
                    Pid -> Pid
                end,
            {ok, check_peer_visibility(State#sess_state{is_peers_visible=true,
                                                        peers_pid=PeersPid})};
         [<<"cascadae">>,<<"Container">>] ->
            {ok, check_peer_visibility(
                 check_file_visibility(State#sess_state{is_page_visible=true}))};
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
recv(_, _, {event, <<>>, <<"d_childrenRequest">>, Meta}, State) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    TorrentID = proplists:get_value(<<"torrent_id">>, Data),
    FileIDs   = proplists:get_value(<<"file_ids">>, Data),
    assert_hash(Hash),
    FilesPid =    %% try start
        case State#sess_state.files_pid of
            undefined ->
                {ok, Pid} = cascadae_files:start_link(self(), {files, Hash}),
                Pid;
            Pid -> Pid
        end,
    cascadae_files:request(FilesPid, TorrentID, FileIDs),
    {ok, State#sess_state{files_pid=FilesPid, is_files_visible=true}};
recv(Session, _, {event, <<>>, <<"d_wishFiles">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        FileIDs   = proplists:get_value(<<"file_ids">>, Data),
        {ok, NewWishes} = etorrent_torrent_ctl:wish_file(TorrentID, FileIDs),
        case State#sess_state.wish_list_hash of
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

recv(_, _, {event, <<>>, <<"d_updateFilters">>, Meta},
     State=#sess_state{peers_pid=PeersPid}) when is_pid(PeersPid) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    TorrentIDs = proplists:get_value(<<"torrent_ids">>, Data),
    assert_hash(Hash),
    assert_list(TorrentIDs),
    cascadae_peers:set_torrent_list(PeersPid, TorrentIDs),
    {ok, State};

recv(Pid, Sid, Message, State) ->
    lager:debug("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    {ok, State}.


handle_info(Pid, _, {torrents, What},
            State=#sess_state{torrent_table_hash=Hash})
    when is_binary(Hash) ->
    case What of
        {diff_list, Rows} ->
            % FIXME
%           fire_data_event(Pid, Hash, <<"rd_dataUpdated">>,
%                           [{<<"rows">>, Rows}]),
            State;
        {add_list, Rows} ->
            fire_data_event(Pid, Hash, <<"rd_dataAdded">>,
                            [{<<"rows">>, Rows}]);
        {delete_list, Rows} ->
            fire_data_event(Pid, Hash, <<"rd_dataRemoved">>,
                            [{<<"rows">>, Rows}])
    end,
    {ok, State};
handle_info(Pid, _, {{peers, Hash}, What}, State=#sess_state{})
    when is_binary(Hash) ->
    lager:info("PEEERS", []),
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
handle_info(Pid, _, {{files, Hash}, What}, State=#sess_state{}) ->
    case What of
        {add_list, TorrentID, Nodes} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Nodes}],
            fire_data_event(Pid, Hash, <<"rd_childrenRespond">>, Data);
        {diff_list, TorrentID, Diff} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Diff}],
            fire_data_event(Pid, Hash, <<"rd_dataUpdated">>, Data)
    end,
    {ok, State};
handle_info(Pid, _, {log_event, Mess}, State = #sess_state{}) ->
    Message = {event, <<"rd_logEvent">>, Mess},
    socketio_session:send(Pid, Message),
    {ok, State};

handle_info(_Pid, _Sid, Info, State = #sess_state{}) ->
    lager:debug("Unhandled message recieved ~p.", [Info]),
    {ok, State}.


close(Pid, Sid, #sess_state{}) ->
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
    store_object(Path, Hash, State#sess_state{torrent_table_hash=Hash});
register_object(Path=[<<"cascadae">>,<<"wishlist">>,<<"List">>], Hash, State) ->
    Session = self(),
    add_event_listener(Session, Hash, <<"d_wishesSave">>),
    add_event_listener(Session, Hash, <<"d_wishesRequest">>),
    store_object(Path, Hash, State#sess_state{wish_list_hash=Hash});
register_object(Path=[<<"cascadae">>,<<"files">>,<<"Tree">>], Hash, State) ->
    Session = self(),
    add_event_listener(Session, Hash, <<"d_childrenRequest">>),
    add_event_listener(Session, Hash, <<"d_wishFiles">>),
    add_event_listener(Session, Hash, <<"d_skipFiles">>),
    add_event_listener(Session, Hash, <<"d_unskipFiles">>),
    add_event_listener(Session, Hash, <<"activated">>),
    add_event_listener(Session, Hash, <<"deactivated">>),
    store_object(Path, Hash, State);
register_object(Path=[<<"cascadae">>,<<"Container">>], Hash, State) ->
    Session = self(),
    add_event_listener(Session, Hash, <<"activated">>),
    add_event_listener(Session, Hash, <<"deactivated">>),
    fire_event(Session, Hash, <<"r_checkVisibility">>),
    store_object(Path, Hash, State);

register_object(Path=[<<"cascadae">>,<<"peers">>,<<"Table">>], Hash, State) ->
    Session = self(),
%   proc_lib:spawn_link(fun() ->
%           Peers = cascadae_peers:all_peers(),
%           fire_data_event(Session, Hash, <<"rd_dataLoadCompleted">>,
%                           [{<<"rows">>, Peers}])
%       end),
    add_event_listener(Session, Hash, <<"activated">>),
    add_event_listener(Session, Hash, <<"deactivated">>),
    add_event_listener(Session, Hash, <<"d_updateFilters">>),
    store_object(Path, Hash, State);
register_object(Path, Hash, State) ->
    lager:warning("Cannot register objest ~p:~p.", [Path, Hash]),
    State.


store_object(Path, Hash, State=#sess_state{obj_tbl=ObjTbl}) ->
    ets:insert_new(ObjTbl, #object{path=Path, hash=Hash}),
    State.

extract_object(Hash, #sess_state{obj_tbl=ObjTbl}) ->
    case ets:lookup(ObjTbl, Hash) of
        [X] -> X
    end.


fire_event(Session, Hash, EventName) ->
    lager:debug("Fire event ~p.", [EventName]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash}],
    Message = {event, <<"fireEvent">>, Args},
    socketio_session:send(Session, Message).


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
assert_list(List) when is_list(List) -> ok.
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



%% The timer must be active only when the page is visible and file view is active.
check_file_visibility(State=#sess_state{is_page_visible=true,
                                        is_files_visible=true,
                                        is_files_active=false,
                                        files_pid=FilesPid}) ->
    cascadae_files:activate(FilesPid),
    State#sess_state{is_files_active=true};
check_file_visibility(State=#sess_state{is_files_active=true,
                                        files_pid=FilesPid}) ->
    cascadae_files:deactivate(FilesPid),
    State#sess_state{is_files_active=false};
check_file_visibility(State) ->
    State.


check_peer_visibility(State=#sess_state{is_page_visible=true,
                                        is_peers_visible=true,
                                        is_peers_active=false,
                                        peers_pid=PeersPid}) ->
    cascadae_peers:activate(PeersPid),
    State#sess_state{is_peers_active=true};
check_peer_visibility(State=#sess_state{is_peers_active=true,
                                        peers_pid=PeersPid}) ->
    cascadae_peers:deactivate(PeersPid),
    State#sess_state{is_peers_active=false};
check_peer_visibility(State) ->
    lager:info("Check visibility failed, page visible = ~p, "
               "peer visible = ~p, peer process active = ~p.",
               [State#sess_state.is_page_visible,
                State#sess_state.is_peers_visible,
                State#sess_state.is_peers_active]),
    State.
