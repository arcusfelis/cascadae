-module(cascadae_trackers).

%% ttid = {torrent_id(), tracker_id()}

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(trackers_state, {
        trackers = dict:new(), %% dict(TrackerID => #tracker{})
        session_pid,
        session_tag,
        session_mref,
        torrent_ids,
        update_table_tref,

        ttids = [],
        %% Loaded trackers on the client side.
        %% Both visible or not.
        cl_ttids = [],
        viz_ttids = [],
        viz_torrent_ids = []
}).

-record(tracker, {
    id,
    last_attempted,
    last_announced,
    message,
    message_level
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         set_torrent_list/2,
         activate/1,
         deactivate/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Session, Tag) ->
    gen_server:start_link(?MODULE, [Session, Tag], []).

set_torrent_list(Srv, TorrentIDs) ->
    gen_server:cast(Srv, {set_torrent_list, TorrentIDs}).

activate(Srv) ->
    gen_server:cast(Srv, activate).

deactivate(Srv) ->
    gen_server:cast(Srv, deactivate).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Session, Tag]) ->
    lager:info("Start cascadae_trackers."),
    {ok, TRef} = timer:send_interval(5000, update_table),
    SMRef = monitor(process, Session),
    State = #trackers_state{
            session_pid=Session,
            session_tag=Tag,
            session_mref=SMRef,
            update_table_tref=TRef
            },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({set_torrent_list, NewTorrentIDs}, State) ->
    {noreply, set_torrent_list_int(NewTorrentIDs, State)};
handle_cast(activate, State=#trackers_state{update_table_tref=undefined}) ->
    {ok, TRef} = timer:send_interval(5000, update_table),
    lager:info("Activate ~p.", [TRef]),
    {noreply, State#trackers_state{update_table_tref=TRef}};
handle_cast(activate, State=#trackers_state{}) ->
    lager:info("Skip activation.", []),
    {noreply, State};
handle_cast(deactivate, State=#trackers_state{update_table_tref=undefined}) ->
    lager:info("Skip deactivation.", []),
    {noreply, State};
handle_cast(deactivate, State=#trackers_state{update_table_tref=TRef}) ->
    lager:info("Deactivate ~p.", [TRef]),
    {ok, cancel} = timer:cancel(TRef),
    {noreply, State#trackers_state{update_table_tref=undefined}}.


handle_info(update_table, State) ->
    lager:debug("Handle update_table timeout.", []),
    {noreply, cron_find_new(State)};
handle_info({'DOWN', MRef, process, _, Reason},
            State=#trackers_state{session_mref=MRef}) ->
    {stop, Reason, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% not active
set_torrent_list_int(NewTorrentIds, S=#trackers_state{update_table_tref=undefined}) ->
    lager:info("Update torrent list ~p => ~p while updates are inactive.",
               [S#trackers_state.viz_torrent_ids, NewTorrentIds]),
    S#trackers_state{viz_torrent_ids=NewTorrentIds};
set_torrent_list_int(NewTorrentIds, S=#trackers_state{ttids=OldSTT,
                                               cl_ttids=OldClSTT}) ->
    self() ! update_table, %% TODO: can be optimized
    lager:info("Update torrent list ~p => ~p.",
               [S#trackers_state.viz_torrent_ids, NewTorrentIds]),
    NotAddedYet = ordsets:subtract(OldSTT, OldClSTT),
    S1 = S#trackers_state{viz_torrent_ids=NewTorrentIds},
    viz_add_trackers(NotAddedYet, S1).


cron_find_new(S=#trackers_state{ttids=OldSTT}) ->
    TT  = etorrent_tracker:all_torrent_and_tracker_ids(),
    STT = lists:usort(TT),
    {DeletedTT, AddedTT} = ordsets_diff(OldSTT, STT),
    S1 = S#trackers_state{ttids=STT},
    S2 = viz_delete_trackers(DeletedTT, S1),
    S3 = viz_add_trackers(AddedTT, S2),
    S4 = viz_diff_trackers(S3),
    S4.


%% Delete rows on the client side.
viz_delete_trackers([], S=#trackers_state{}) ->
    S;
viz_delete_trackers(DeletedTT,
                 S=#trackers_state{cl_ttids=OldClSTT, viz_ttids=OldVizSTT,
                                trackers=Trackers}) ->
    lager:info("Deleted trackers ~p.", [DeletedTT]),
    NewClSTT = ordsets:subtract(OldClSTT, DeletedTT),
    NewVizSTT = ordsets:subtract(OldVizSTT, DeletedTT),
    ClDeletedTT = ordsets:subtract(DeletedTT, OldClSTT),
    case ClDeletedTT of
        [] -> S;
        [_|_] ->
            %% Push ClDeletedTT to the client.
            lager:info("Delete ~p from the client tracker table.", [ClDeletedTT]),
            TrackerIDs = [TrackerID || TrackerID <- ClDeletedTT],
            push_to_client({delete_list, TrackerIDs}, S),
            NewTrackers = clean_trackers(ClDeletedTT, Trackers),
            S#trackers_state{cl_ttids=NewClSTT, viz_ttids=NewVizSTT,
                          trackers=NewTrackers}
    end.

%% Add requested rows to the client 
viz_add_trackers([], S=#trackers_state{}) ->
    S;
viz_add_trackers(AddedTT, S=#trackers_state{cl_ttids=OldClSTT,
                                            viz_ttids=OldVizSTT,
                                            viz_torrent_ids=VizTorrents}) ->
    lager:info("Added new trackers ~p.", [AddedTT]),
    %% Push VizAddedSTT to the client.
    VizAddedSTT = filter_visible_trackers(AddedTT, VizTorrents),
    case VizAddedSTT of
        [] -> S;
        [_|_] ->
            NewClSTT = ordsets:union(OldClSTT, VizAddedSTT),
            lager:info("Add ~p to the client tracker table.", [VizAddedSTT]),
            {List, S1} = tracker_list(VizAddedSTT, S),
            push_to_client({add_list, List}, S),
            NewVizSTT = ordsets:union(OldVizSTT, VizAddedSTT),
            S1#trackers_state{cl_ttids=NewClSTT, viz_ttids=NewVizSTT}
    end.


push_to_client(Mess, #trackers_state{ session_pid=Session, session_tag=Tag }) ->
    lager:info("Send ~p to ~p.", [{Tag, Mess}, Session]),
    cascadae_session:send(Session, {Tag, Mess}).


%% Empty list means all torrents are visible.
filter_visible_trackers(AddedTT, []) ->
    AddedTT;
filter_visible_trackers(AddedTT, VizTorrents) ->
    lager:info("Filter visible trackers, visible torrents are ~p.", [VizTorrents]),
    orddict_with_set_intersection(AddedTT, VizTorrents).

orddict_with_set_intersection([{K,V}|Dict], [K|Set]) ->
    [{K,V}|orddict_with_set_intersection(Dict, Set)];
orddict_with_set_intersection([{DK,_V}|Dict], [SK|_]=Set) when DK < SK ->
    %% Skip {DK,V}
    orddict_with_set_intersection(Dict, Set);
orddict_with_set_intersection([_|_]=Dict, [_|Set]) ->
    %% Skip SK
     orddict_with_set_intersection(Dict, Set);
orddict_with_set_intersection(_, _) ->
     [].



ordsets_diff(Olds, News) ->
    ordsets_diff(Olds, News, [], []).

ordsets_diff([X|Olds], [X|News], Added, Deleted) ->
    ordsets_diff(Olds, News, Added, Deleted);
ordsets_diff([Old|Olds], [New|News], Added, Deleted) when Old < New ->
    ordsets_diff(Olds, [New|News], Added, [Old|Deleted]);
ordsets_diff([Old|Olds], [New|News], Added, Deleted) ->
    ordsets_diff([Old|Olds], News, [New|Added], Deleted);
ordsets_diff(Olds, News, Added, Deleted) ->
    {lists:reverse(Deleted, Olds), lists:reverse(Added, News)}.



tracker_list(TTs, S) ->
    tracker_list(TTs, S, []).

tracker_list([{TorrentID, TrackerID}|TTs], S, Acc) ->
    case get_tracker(TorrentID, TrackerID, S) of
        {undefined, S1} -> tracker_list(TTs, S1, Acc);
        {ObjJSON, S1} -> tracker_list(TTs, S1, [ObjJSON|Acc])
    end;
tracker_list([], S, Acc) ->
    {lists:reverse(Acc), S}.


get_tracker(TorrentID, TrackerID, S=#trackers_state{trackers=Trackers}) ->
     case etorrent_tracker:lookup(TrackerID) of
        {value, PL} ->
            Lvl       = proplists:get_value(message_level, PL),
            Message   = proplists:get_value(message, PL),
            Announced = proplists:get_value(last_announced, PL),
            Attempted = proplists:get_value(last_attempted, PL),
            ObjJSON =
                [{torrent_id, TorrentID}
                ,{id, TrackerID}
                ,{tracker_url, list_to_binary(
                            proplists:get_value(tracker_url, PL))}
                ,{tier_num, proplists:get_value(tier_num, PL)}

                ,{message_level, atom_to_binary(Lvl, utf8)}
                ,{message, Message}
                ,{last_announced, seconds_diff_from_now(Announced)}
                ,{last_attempted, seconds_diff_from_now(Attempted)}
                ],
            Tracker = #tracker{
                    id=TrackerID,
                    message=Message,
                    message_level=Lvl,
                    last_attempted=Attempted,
                    last_announced=Announced
            },
            NewTrackers = dict:store(TrackerID, Tracker, Trackers),
            {filter_undefined(ObjJSON), S#trackers_state{trackers=NewTrackers}};
        not_found -> {undefined, S}
    end.

filter_undefined(ObjJSON) ->
    [{K,V} || {K,V} <- ObjJSON, V =/= undefined].


clean_trackers([TT|TTs], Trackers) ->
    clean_trackers(TTs, dict:erase(TT, Trackers));
clean_trackers([], Trackers) ->
    Trackers.


viz_diff_trackers(S=#trackers_state{viz_ttids=[]}) -> S;
viz_diff_trackers(S=#trackers_state{viz_ttids=VizSTT, trackers=Trackers}) ->
    {Diff, NewTrackers} = diff_trackers(VizSTT, Trackers, []),
    case Diff of
        [] -> S;
        [_|_] ->
            lager:info("Diff ~p", [Diff]),
            push_to_client({diff_list, Diff}, S),
            S#trackers_state{trackers=NewTrackers}
    end.

diff_trackers([{TorrentID,TrackerID}|TTs], Trackers, Diffs) ->
    case dict:find(TrackerID, Trackers) of
        {ok, OldTracker} ->
            case diff_tracker(TorrentID, TrackerID, OldTracker) of
                {changed, Diff, NewTracker} ->
                    Trackers2 = dict:store(TrackerID, NewTracker, Trackers),
                    diff_trackers(TTs, Trackers2, [Diff|Diffs]);
                same ->
                    diff_trackers(TTs, Trackers, Diffs)
            end;

        error ->
            diff_trackers(TTs, Trackers, Diffs)
    end;
diff_trackers([], Trackers, Diffs) ->
    {Diffs, Trackers}.



diff_tracker(_TorrentID, TrackerID, OldTracker) ->
    case etorrent_tracker:lookup(TrackerID) of
        {value, PL} ->
            Lvl       = proplists:get_value(message_level, PL),
            Message   = proplists:get_value(message, PL),
            Announced = proplists:get_value(last_announced, PL),
            Attempted = proplists:get_value(last_attempted, PL),
            NewTracker = #tracker{
                id=TrackerID,
                message=Message,
                message_level=Lvl,
                last_attempted=Attempted,
                last_announced=Announced
            },
            case NewTracker of
                OldTracker -> same;
                _ ->
                    #tracker{
                        message=OldMessage,
                        message_level=OldLvl,
                        last_attempted=OldAttempted,
                        last_announced=OldAnnounced
                    } = OldTracker,
                    Diff = [{id, TrackerID}]
                        ++ [{message, Message} || OldMessage =/= Message]
                        ++ [{message_level, Lvl} || OldLvl =/= Lvl]
                        ++ [{last_attempted, seconds_diff_from_now(Attempted)}
                            || OldAttempted =/= Attempted]
                        ++ [{last_announced, seconds_diff_from_now(Announced)}
                            || OldAnnounced =/= Announced],
                    {changed, Diff, NewTracker}
            end;
        not_found -> same
    end.

seconds_diff_from_now(undefined) ->
    undefined;
seconds_diff_from_now(X) ->
    timestamp_to_seconds(os:timestamp()) - timestamp_to_seconds(X).

timestamp_to_seconds({MegaSecs, Secs, _}) ->
    MegaSecs * 1000000 + Secs.

