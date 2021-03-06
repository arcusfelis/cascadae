-module(cascadae_session).
-compile({parse_transform, seqbind}).

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
    is_files_active = false,
    is_files_visible,
    peers_pid,
    is_peers_active = false,
    is_peers_visible,
    trackers_pid,
    is_trackers_active = false,
    is_trackers_visible,
    is_page_visible=true,
    session_timeout_tref,
    saved_hub_state,
    saved_speed_hub_state,
    speed_ctrl_hash
}).
-define(OBJ_TBL, cascadae_object_register).

-record(object, {
    hash,
    path
}).

%% API
send(SPid, Mess) when is_pid(SPid) ->
    SPid ! Mess.

%% ---- Handlers
open(SPid, Sid, _Opts) ->
    put(process_type, cascadae_session),
    %% Initialization
    cascadae_hub:add_handler(),
    error_logger:info_msg("open ~p ~p~n", [SPid, Sid]),
    ObjTbl = ets:new(?OBJ_TBL, [{keypos, #object.hash}]),
    {ok, #sess_state{obj_tbl=ObjTbl}}.


recv(SPid, _Sid, _, #sess_state{}) when SPid =/= self() ->
    error_logger:error_msg("FUUUU", []),
    error(haha);
recv(_SPid, _Sid, {json, <<>>, Json}, State = #sess_state{}) ->
    lager:debug("recv json ~p~n", [Json]),
    {ok, State};

recv(_SPid, _Sid, {message, <<>>, _Message}, State = #sess_state{}) ->
%%  socketio_session:send_message(Pid, Message),
    {ok, State};


recv(_, _, {event, <<>>, <<"changePageVisible">>, Meta}, State@) ->
    IsVisible = proplists:get_value(<<"data">>, Meta),
    assert_bool(IsVisible),
    State@ = State@#sess_state{is_page_visible=IsVisible},
    State@ = check_hub_state(State@),
    State@ = check_speed_hub_state(State@),
    State@ = check_tracker_visibility(State@),
    State@ = check_peer_visibility(State@),
    State@ = check_file_visibility(State@),
    {ok, State@};

recv(SPid, _, {event, <<>>, <<"changeActive">>, Meta}=Mess, State) ->
    #sess_state{files_pid=FilesPid} = State,
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_hash(Hash),
    IsVisible = proplists:get_value(<<"data">>, Meta),
    assert_bool(IsVisible),
    #object{path=Path} = extract_object(Hash, State),
    if IsVisible ->
    %% activated
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
                        {ok, Pid} = cascadae_peers:start_link(SPid,
                                                              {peers, Hash}),
                        Pid;
                    Pid -> Pid
                end,
            {ok, check_peer_visibility(State#sess_state{is_peers_visible=true,
                                                        peers_pid=PeersPid})};
         [<<"cascadae">>,<<"trackers">>,<<"Table">>] ->
            %% It is only meaningful, if nothing is selected.
            %% Start here.
            TrackersPid = %% try start
                case State#sess_state.trackers_pid of
                    undefined ->
                        {ok, Pid} = cascadae_trackers:start_link(SPid,
                                         {trackers, Hash}),
                        Pid;
                    Pid -> Pid
                end,
            {ok, check_tracker_visibility(State#sess_state{
                        is_trackers_visible=true, trackers_pid=TrackersPid})};
         _ ->
            lager:debug("Ignore ~p.", [Mess]),
            {ok, State}
    end;
    true ->
    #sess_state{peers_pid=PeersPid, trackers_pid=TrackersPid} = State,
    %% deactivated
    case Path of
         [<<"cascadae">>,<<"files">>,<<"Tree">>] when is_pid(FilesPid) ->
            {ok, check_file_visibility(State#sess_state{is_files_visible=false})};
         [<<"cascadae">>,<<"peers">>,<<"Table">>] when is_pid(PeersPid) ->
            {ok, check_peer_visibility(State#sess_state{is_peers_visible=false})};
         [<<"cascadae">>,<<"trackers">>,<<"Table">>] when is_pid(TrackersPid) ->
            {ok, check_tracker_visibility(
                    State#sess_state{is_trackers_visible=false})};
         _ ->
            lager:debug("Ignore ~p.", [Mess]),
            {ok, State}
    end
    end;

recv(SPid, _, {event, <<>>, <<"registerObject">>, Meta}, State) ->
    Path = proplists:get_value(<<"path">>, Meta),
    Hash = proplists:get_value(<<"hash">>, Meta),
    assert_path(Path),
    assert_hash(Hash),
    {ok, register_object(SPid, Path, Hash, State)};

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
recv(SPid, _, {event, <<>>, <<"d_requestMagnetLinks">>, Meta}, State) ->
    Hash = proplists:get_value(<<"hash">>, Meta),
    Data = proplists:get_value(<<"data">>, Meta),
    Ids = proplists:get_value(<<"torrent_ids">>, Data),
    Links = [{int_to_bin(Id), etorrent_info:magnet_link(Id)} || Id <- Ids],
    RespondData = [{<<"magnet_links">>, Links}],
    fire_data_event(SPid, Hash, <<"rd_respondMagnetLinks">>, RespondData),
    {ok, State};

%% FILES
%% Request children of the file tree.
recv(SPid, _, {event, <<>>, <<"d_childrenRequest">>, Meta}, State) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    TorrentID = proplists:get_value(<<"torrent_id">>, Data),
    FileIDs   = proplists:get_value(<<"file_ids">>, Data),
    assert_hash(Hash),
    FilesPid =    %% try start
        case State#sess_state.files_pid of
            undefined ->
                {ok, PPid} = cascadae_files:start_link(SPid, {files, Hash}),
                PPid;
            PPid -> PPid
        end,
    cascadae_files:request(FilesPid, TorrentID, FileIDs),
    {ok, State#sess_state{files_pid=FilesPid, is_files_visible=true}};
recv(SPid, _, {event, <<>>, <<"d_wishFiles">>, Meta}, State) ->
    spawn_link(fun() ->
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        FileIDs   = proplists:get_value(<<"file_ids">>, Data),
        {ok, NewWishes} = etorrent_torrent_ctl:wish_file(TorrentID, FileIDs),
        case State#sess_state.wish_list_hash of
            undefined -> ok;
            WLHash ->
                RespondData = encode_wishes(TorrentID, NewWishes), 
                fire_data_event(SPid, WLHash, <<"rd_wishesRespond">>,
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
recv(SPid, _, {event, <<>>, <<"d_wishesRequest">>, Meta}, State) ->
    spawn_link(fun() ->
        Hash      = proplists:get_value(<<"hash">>, Meta),
        Data      = proplists:get_value(<<"data">>, Meta),
        TorrentID = proplists:get_value(<<"torrent_id">>, Data),
        {ok, Wishes} = etorrent_torrent_ctl:get_wishes(TorrentID),
        RespondData = encode_wishes(TorrentID, Wishes), 
        fire_data_event(SPid, Hash, <<"rd_wishesRespond">>, RespondData) 
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

recv(_, _, {event, <<>>, <<"submitData">>, Meta}, State=#sess_state{}) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
       [<<"cascadae">>,<<"AddTorrentWindow">>] ->
            Address   = proplists:get_value(<<"address">>, Data),
            Paused    = proplists:get_value(<<"paused">>, Data),
            IsPaused = if Paused -> true; true -> false end,
            [lager:debug("Add ~p on pause.", [Address]) || IsPaused],
            proc_lib:spawn(fun() ->
                etorrent_magnet:download({address, Address}, [{paused, IsPaused}])
                end),
            {ok, State}
    end;

recv(_, _, {event, <<>>, <<"d_changeRates">>, Meta}, State=#sess_state{}) ->
    Hash      = proplists:get_value(<<"hash">>, Meta),
    Data      = proplists:get_value(<<"data">>, Meta),
    #object{path=Path} = extract_object(Hash, State),
    case Path of
       [<<"cascadae">>,<<"speedControl">>,<<"Info">>] ->
            In  = proplists:get_value(<<"rate_in">>, Data),
            Out = proplists:get_value(<<"rate_out">>, Data),
            proc_lib:spawn(fun() ->
                  etorrent_rlimit:max_send_rate(Out),
                  etorrent_rlimit:max_recv_rate(In)
                end),
            {ok, State}
    end;

recv(SPid, Sid, Message, State) ->
    lager:debug("recv ~p ~p ~p~n", [SPid, Sid, Message]),
    {ok, State}.


handle_info(SPid, _, {speed_rate_update, ChangedPL},
            State=#sess_state{speed_ctrl_hash=Hash}) ->
    fire_data_event(SPid, Hash, <<"rd_dataUpdated">>, ChangedPL),
    lager:debug("Speed: ~p", [ChangedPL]),
    {ok, State};
handle_info(SPid, _, {torrents, What},
            State=#sess_state{torrent_table_hash=Hash})
    when is_binary(Hash) ->
    case What of
        {diff_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataUpdated">>,
                            [{<<"rows">>, Rows}]),
            State;
        {add_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataAdded">>,
                            [{<<"rows">>, Rows}]);
        {delete_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataRemoved">>,
                            [{<<"rows">>, Rows}])
    end,
    {ok, State};
handle_info(SPid, _, {{peers, Hash}, What}, State=#sess_state{})
    when is_binary(Hash) ->
    case What of
        {diff_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataUpdated">>,
                            [{<<"rows">>, Rows}]);
        {set_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataLoadCompleted">>,
                            [{<<"rows">>, Rows}]);
        {add_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataAdded">>,
                            [{<<"rows">>, Rows}]);
        {delete_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataRemoved">>,
                            [{<<"rows">>, Rows}])
    end,
    {ok, State};
handle_info(SPid, _, {{trackers, Hash}, What}, State=#sess_state{})
    when is_binary(Hash) ->
    case What of
        {diff_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataUpdated">>,
                            [{<<"rows">>, Rows}]);
        {set_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataLoadCompleted">>,
                            [{<<"rows">>, Rows}]);
        {add_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataAdded">>,
                            [{<<"rows">>, Rows}]);
        {delete_list, Rows} ->
            fire_data_event(SPid, Hash, <<"rd_dataRemoved">>,
                            [{<<"rows">>, Rows}])
    end,
    {ok, State};
handle_info(SPid, _, {{files, Hash}, What}, State=#sess_state{}) ->
    case What of
        {add_list, TorrentID, Nodes} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Nodes}],
            fire_data_event(SPid, Hash, <<"rd_childrenRespond">>, Data);
        {diff_list, TorrentID, Diff} ->
            Data = [{<<"torrent_id">>, TorrentID}, {<<"nodes">>, Diff}],
            fire_data_event(SPid, Hash, <<"rd_dataUpdated">>, Data)
    end,
    {ok, State};
handle_info(SPid, _, {log_event, Mess}, State = #sess_state{}) ->
    Message = {event, <<"rd_logEvent">>, Mess},
    socketio_session:send(SPid, Message),
    {ok, State};

handle_info(_SPid, _Sid, Info, State = #sess_state{}) ->
    lager:debug("Unhandled message recieved ~p.", [Info]),
    {ok, State}.


close(SPid, Sid, #sess_state{}) ->
    %% Termination.
    lager:debug("close ~p ~p~n", [SPid, Sid]),
    ok.


%% ------------------------------------------------------------------

register_object(SPid, Path=[<<"cascadae">>,<<"Table">>], Hash, State) ->
    proc_lib:spawn_link(fun() ->
            Torrents = cascadae_hub:all_torrents(),
            fire_data_event(SPid, Hash, <<"rd_dataLoadCompleted">>,
                            [{<<"rows">>, Torrents}])
        end),
    store_object(Path, Hash, State#sess_state{torrent_table_hash=Hash});
register_object(SPid, Path=[<<"cascadae">>,<<"speedControl">>,<<"Info">>],
                Hash, State) ->
    cascadae_speed_hub:add_handler(),
    store_object(Path, Hash, State#sess_state{speed_ctrl_hash=Hash});
register_object(_, Path=[<<"cascadae">>,<<"wishlist">>,<<"List">>], Hash, State) ->
    store_object(Path, Hash, State#sess_state{wish_list_hash=Hash});
register_object(_, Path=[<<"cascadae">>,<<"files">>,<<"Tree">>], Hash, State) ->
    store_object(Path, Hash, State);
register_object(_, Path=[<<"cascadae">>,<<"Container">>], Hash, State) ->
    store_object(Path, Hash, State);
register_object(_, Path=[<<"cascadae">>,<<"peers">>,<<"Table">>], Hash, State) ->
    store_object(Path, Hash, State);
register_object(_, Path=[<<"cascadae">>,<<"trackers">>,<<"Table">>], Hash, State) ->
    store_object(Path, Hash, State);

register_object(_, Path=[<<"cascadae">>,<<"AddTorrentWindow">>], Hash, State) ->
    store_object(Path, Hash, State);

register_object(_SPid, Path, Hash, State) ->
    lager:warning("Cannot register objest ~p:~p.", [Path, Hash]),
    State.


store_object(Path, Hash, State=#sess_state{obj_tbl=ObjTbl}) ->
    ets:insert_new(ObjTbl, #object{path=Path, hash=Hash}),
    State.

extract_object(Hash, #sess_state{obj_tbl=ObjTbl}) ->
    case ets:lookup(ObjTbl, Hash) of
        [X] -> X
    end.


fire_event(SPid, Hash, EventName) ->
    lager:debug("Fire event ~p.", [EventName]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash}],
    Message = {event, <<"fireEvent">>, Args},
    socketio_session:send(SPid, Message).


fire_data_event(SPid, Hash, EventName, EventData) ->
    lager:debug("Fire data event ~p with ~p.", [EventName, EventData]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash},
            {<<"data">>, EventData}],
    Message = {event, <<"fireDataEvent">>, Args},
    socketio_session:send(SPid, Message).


add_event_listener(SPid, Hash, EventName) ->
    lager:debug("Add event listener ~p.", [EventName]),
    Args = [{<<"name">>, EventName},
            {<<"hash">>, Hash}],
    Message = {event, <<"addListener">>, Args},
    socketio_session:send(SPid, Message).



assert_path(Path) when is_list(Path) -> ok.
assert_list(List) when is_list(List) -> ok.
assert_hash(Hash) when is_binary(Hash) -> ok.
assert_bool(Bool) when is_boolean(Bool) -> ok.



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


check_tracker_visibility(State=#sess_state{is_page_visible=true,
                                           is_trackers_visible=true,
                                           is_trackers_active=false,
                                           trackers_pid=TrackerPid}) ->
    cascadae_trackers:activate(TrackerPid),
    State#sess_state{is_trackers_active=true};
check_tracker_visibility(State=#sess_state{is_trackers_active=true,
                                           trackers_pid=TrackerPid}) ->
    cascadae_trackers:deactivate(TrackerPid),
    State#sess_state{is_trackers_active=false};
check_tracker_visibility(State) ->
    lager:info("Check visibility failed, page visible = ~p, "
               "trackers visible = ~p, trackers process active = ~p.",
               [State#sess_state.is_page_visible,
                State#sess_state.is_trackers_visible,
                State#sess_state.is_trackers_active]),
    State.


%% Suspend and resume the hub.
check_hub_state(S=#sess_state{is_page_visible=true, saved_hub_state=undefined}) -> S;
check_hub_state(S=#sess_state{is_page_visible=true, saved_hub_state=SavedHubState}) -> 
    [cascadae_hub:resume_handler(SavedHubState)
     || SavedHubState =/= undefined],
    S#sess_state{saved_hub_state=undefined};
check_hub_state(S=#sess_state{is_page_visible=false, saved_hub_state=undefined}) -> 
    SavedHubState = cascadae_hub:suspend_handler(),
    S#sess_state{saved_hub_state=SavedHubState};
check_hub_state(S=#sess_state{is_page_visible=false}) -> S.

%% Suspend and resume the hub.
check_speed_hub_state(S=#sess_state{speed_ctrl_hash=undefined}) -> S;
check_speed_hub_state(S=#sess_state{is_page_visible=true, saved_speed_hub_state=undefined}) -> S;
check_speed_hub_state(S=#sess_state{is_page_visible=true, saved_speed_hub_state=SavedHubState}) -> 
    [cascadae_speed_hub:resume_handler(SavedHubState)
     || SavedHubState =/= undefined],
    S#sess_state{saved_speed_hub_state=undefined};
check_speed_hub_state(S=#sess_state{is_page_visible=false, saved_speed_hub_state=undefined}) -> 
    SavedHubState = cascadae_speed_hub:suspend_handler(),
    S#sess_state{saved_speed_hub_state=SavedHubState};
check_speed_hub_state(S=#sess_state{is_page_visible=false}) -> S.


int_to_bin(X) ->
    list_to_binary(integer_to_list(X)).
