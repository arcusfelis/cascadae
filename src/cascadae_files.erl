-module(cascadae_files).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(files_state, {
        session_pid,
        session_tag,
        session_mref,
        torrent_id,
        files,
        update_tree_tref
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         request/3,
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

request(Srv, TorrentID, ParentFileIDs) ->
    gen_server:cast(Srv, {request, TorrentID, ParentFileIDs}).

activate(Srv) ->
    gen_server:cast(Srv, activate).

deactivate(Srv) ->
    gen_server:cast(Srv, deactivate).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Session, Tag]) ->
    lager:info("Start cascadae_files."),
    SMRef = monitor(process, Session),
    State = #files_state{
            session_pid=Session,
            session_tag=Tag,
            session_mref=SMRef
            },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(activate, State=#files_state{update_tree_tref=undefined}) ->
    lager:info("Activate timer.", []),
    {ok, TRef} = timer:send_interval(5000, update_tree),
    {noreply, State#files_state{update_tree_tref=TRef}};
handle_cast(activate, State=#files_state{}) ->
    {noreply, State};
handle_cast(deactivate, State=#files_state{update_tree_tref=undefined}) ->
    {noreply, State};
handle_cast(deactivate, State=#files_state{update_tree_tref=TRef}) ->
    lager:info("Deactivate timer.", []),
    {ok, cancel} = timer:cancel(TRef),
    {noreply, State#files_state{update_tree_tref=undefined}};
%% Same torrent.
handle_cast({request, TorrentID, ParentFileIDs},
            State=#files_state{torrent_id=TorrentID}) ->
    #files_state{
        files=Files,
        session_pid=Session,
        session_tag=Tag
        } = State,
    {Nodes, NewFiles} = tree_children(TorrentID, ParentFileIDs),
    cascadae_session:send(Session, {Tag, {add_list, TorrentID, Nodes}}),
    {noreply, State#files_state{files=NewFiles++Files}};

%% New torrent.
handle_cast({request, TorrentID, ParentFileIDs}, State=#files_state{}) ->
    #files_state{
        session_pid=Session,
        session_tag=Tag
        } = State,
    {Nodes, NewFiles} = tree_children(TorrentID, ParentFileIDs),
    cascadae_session:send(Session, {Tag, {add_list, TorrentID, Nodes}}),
    {noreply, State#files_state{torrent_id=TorrentID, files=NewFiles}}.

handle_info(update_tree, State=#files_state{torrent_id=undefined}) ->
    {noreply, State};
handle_info(update_tree, State) ->
    lager:debug("Handle update_tree timeout.", []),
    #files_state{
        torrent_id=TorrentID,
        files=Files,
        session_pid=Session,
        session_tag=Tag
        } = State,
    {Diff, Files2} = etorrent_info:file_diff(TorrentID, Files),
    case Diff of
        [] -> ok;
        [_|_] ->
            cascadae_session:send(Session, {Tag, {diff_list, TorrentID, Diff}})
    end,
    Files == Files2
    orelse lager:debug("Diff state changed ~p => ~p.", [Files, Files2]),
    {noreply, State#files_state{files=Files2}};
handle_info({'DOWN', MRef, process, _, Reason},
            State=#files_state{session_mref=MRef}) ->
    {stop, Reason, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

tree_children(TorrentID, ParentFileIDs) ->
    tree_children(TorrentID, ParentFileIDs, [], []).

tree_children(TorrentID, [ParentFileID|ParentFileIDs], Nodes, Files) ->
    Children = etorrent_info:tree_children(TorrentID, ParentFileID),
    Node = [ {parent_id, ParentFileID}
           , {children, Children} ],
    tree_children(TorrentID, ParentFileIDs, [Node|Nodes], Children ++ Files);
tree_children(_, _, Nodes, Files) ->
    {lists:reverse(Nodes), lists:reverse(Files)}.
