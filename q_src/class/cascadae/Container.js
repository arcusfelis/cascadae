
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("cascadae.Container",
{
  extend : qx.ui.container.Composite,




  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function(app, socket)
  {
    this.__socket = socket;

    // Create main layout
    this.__mainLayout = new qx.ui.layout.Dock();
    this.base(arguments, this.__mainLayout);
    this.__application = app;
    this._initializeCommands();

    socket.addListener("connect", this.__setEnabled, this);
    socket.addListener("disconnect", this.__setDisabled, this);

    // Create header
    this.__header = new cascadae.Header();
    this.add(this.__header, { edge : "north" });

    // Create toolbar
    this.__toolBar = new cascadae.ToolBar(this);
    this.add(this.__toolBar, { edge : "north" });

    // Create side panel
    this.__stack = new qx.ui.container.Stack;
    this.__stack.resetSelection();
    this.__stack.exclude();

    this.__mainStack = new qx.ui.container.Stack;
    this.__table = new cascadae.Table();
    this.__socket.addListener("rd_logEvent", this.__table.logHandler, this.__table);
    this.__mainStack.add(this.__table);
    socket.registerObject(this.__table);


    this.__infosplit = new qx.ui.splitpane.Pane("horizontal");
    this.__infosplit.setDecorator(null);
    this.add(this.__infosplit);

    this.__infosplit.add(this.__mainStack, 2);
    this.__infosplit.add(this.__stack, 1);

    this._initToolbarButtonActivation();
//  this._initToolbarFileButtonActivation();

    this.setEnabled(false);

    // Set "dead" zones (never will get focus)
    this.__header.setKeepFocus(true);
    this.__toolBar.setKeepFocus(true);
  },

  members :
  {
    __mainLayout : null,
    __header : null,
    __toolBar : null,
    __socket : null,
    __table : null,
    __commands : null,
    __application : null,
    __viewLoaded : false,
    __activeView : "",

    getRoot : function()
    {
      return this.__application.getRoot();
    },

    finalize : function()
    {
      var container = this;

      // Do heavy calculations in idle time
      // document.setTimeout
      qx.event.Timer.once(container._initViews, container, 3000);
    },

    _initViews : function()
    {
      if (!this.__table) throw("Init the main table first!");

      this.__peersTable = new cascadae.peers.Table();
      this.__peersTable.initFilters(this.__table);
      this.__socket.registerObject(this.__peersTable);

      this.__logTable = new cascadae.log.Table();
      this.__logTable.initFilters(this.__table);
      this.__socket.registerObject(this.__logTable);
      this.__socket.addListener("rd_logEvent", this.__logTable.logHandler, this.__logTable);

      this.__filesTree = new cascadae.files.Tree();
      this.__socket.registerObject(this.__filesTree);

      // There is property binding here.
      // If table.torrentId will changed, than __filesTree.torrentId will be
      // changed too.
      this.__table.bind("torrentId", this.__filesTree, "torrentId");

      this.__wishesList = new cascadae.wishlist.List();
      this.__table.bind("torrentId", this.__wishesList, "torrentId");
      this.__socket.registerObject(this.__wishesList);

      this._initToolbarFileButtonActivation();
 
      this.__filesView = this.__filesTree;
      this.__wishesView = this.__wishesList;
      this.__peersView = this.__peersTable;
      this.__logView = this.__logTable;
 
      this.__stack.add(this.__filesView);
      this.__stack.add(this.__wishesView);
      this.__stack.add(this.__peersView);
      this.__stack.add(this.__logView);

      this.__regFocusHandlers([ this.__table
                              , this.__filesTree 
                              , this.__peersTable 
                              , this.__logTable   
                              , this.__wishesList
                              ]);

      this.__viewLoaded = true;
      this.selectView(this.__activeView);
    },

    __regFocusHandlers : function(arr)
    {
      var container = this;

      arr.map(function(obj) {
        obj.addListener("focusin",  container.__focusInHandler,  container);
        obj.addListener("focusout", container.__focusOutHandler, container);
      });
    },  

    __focusInHandler : function(e)
    {
      var t = e.getTarget();
      var n = this.getName(t);
      this.__toolBar.activate(n);
    },

    __focusOutHandler : function(e)
    {
      var t = e.getTarget();
      var n = this.getName(t);
      this.__toolBar.deactivate(n);
    },

    getName: function(t)
    {
      return (t == this.__table)       ? "torrent_table" : 
             (t == this.__filesTree)   ? "file_tree"     : 
             (t == this.__peersTable)  ? "peer_table"    : 
             (t == this.__logTable)    ? "log_table"     : 
             (t == this.__wishesList)  ? "wish_list"     : 
             "unknown";
    },


    /**
     * TODOC
     *
     */
    _initToolbarButtonActivation : function()
    {
      var selModel = this.__table.getSelectionModel();

      // Register action for enabling the buttons when
      // a row is selected.
      selModel.addListener("changeSelection", function(e)
      {
        var isEnabled = !(e.getCurrentTarget().isSelectionEmpty());
        this.enableRowButtons(isEnabled);
      },
      this.__toolBar);
    },


    _initToolbarFileButtonActivation : function()
    {
      var selModel = this.__filesTree.getSelectionModel();

      // Register action for enabling the buttons when
      // a row is selected.
      selModel.addListener("changeSelection", function(e)
      {
        var isEnabled = !(e.getCurrentTarget().isSelectionEmpty());
        this.enableFileRowButtons(isEnabled);
      },
      this.__toolBar);
    },


    /**
     * Delete selected rows from the table
     *
     */
    removeSelectedRows : function() {
      this.__table.removeSelectedRows();
    },


    /**
     * TODOC
     *
     */
    startSelectedRows : function() {
      this.__table.startSelectedRows();
    },


    /**
     * TODOC
     *
     */
    stopSelectedRows : function() {
      this.__table.stopSelectedRows();
    },


    /**
     * TODOC
     *
     */
    reconnect : function() {
      this.__socket.reconnect();
    },


    /**
     * TODOC
     *
     */
    reload : function() {
      this.__socket.reload();
    },


    /**
     * TODOC
     *
     */
    showAddTorrentDialog : function() {
      var win = new cascadae.AddTorrentWindow();
      this.getRoot().add(win);
      win.show();
//    win.focus();
      win.addListener("close", this.__table.focus, this.__table);
      win.addListener("submitData", this.__addTorrentSubmit, this);
    },

    __addTorrentSubmit : function(e) {
        this.info("TODO");
//      this.__store.addTorrent(e.getData());
    },


    /**
     * Get the command with the given command id
     *
     * @param commandId {String} the command's command id
     * @return {qx.ui.core.Command} The command
     */
    getCommand : function(commandId) {
      return this.__commands[commandId];
    },


    /**
     * Initialize commands (shortcuts, ...)
     *
     */
    _initializeCommands : function()
    {
      var commands = {};

      commands.reconnect = new qx.ui.core.Command("Control+Shift+R");
      commands.reconnect.setToolTipText("Control+Shift+R");
      commands.reconnect.addListener("execute", this.reconnect, this);

      commands.reload = new qx.ui.core.Command("Control+R");
      commands.reload.setToolTipText("Control+R");
      commands.reload.addListener("execute", this.reload, this);

      commands.addTorrent = new qx.ui.core.Command("Control+A");
      commands.addTorrent.setToolTipText("Control+A");
      commands.addTorrent.addListener("execute", this.showAddTorrentDialog, this);

      commands.removeSelectedRows = new qx.ui.core.Command("Control+D");
      commands.removeSelectedRows.setToolTipText("Control+D");
      commands.removeSelectedRows.addListener("execute", this.removeSelectedRows, this);

      commands.startSelectedRows = new qx.ui.core.Command("Control+S");
      commands.startSelectedRows.setToolTipText("Control+S");
      commands.startSelectedRows.addListener("execute", this.startSelectedRows, this);

      commands.stopSelectedRows = new qx.ui.core.Command("Control+P");
      commands.stopSelectedRows.setToolTipText("Control+P");
      commands.stopSelectedRows.addListener("execute", this.stopSelectedRows, this);

      commands.wishSelectedFiles = new qx.ui.core.Command("Control+W");
      commands.wishSelectedFiles.setToolTipText("Control+W");
      commands.wishSelectedFiles.addListener("execute", this.wishSelectedFiles, this);

      commands.skipSelectedFiles = new qx.ui.core.Command("Control+K");
      commands.skipSelectedFiles.setToolTipText("Control+K");
      commands.skipSelectedFiles.addListener("execute", this.skipSelectedFiles, this);

      commands.unskipSelectedFiles = new qx.ui.core.Command("Control+U");
      commands.unskipSelectedFiles.setToolTipText("Control+U");
      commands.unskipSelectedFiles.addListener("execute", this.unskipSelectedFiles, this);


      // Special commands
      commands.showWishView = new qx.ui.core.Command("Control+Shift+W");
      commands.showWishView.setToolTipText("Control+Shift+W");

      commands.showLogView = new qx.ui.core.Command("Control+Shift+L");
      commands.showLogView.setToolTipText("Control+Shift+L");

      commands.showFileView = new qx.ui.core.Command("Control+Shift+F");
      commands.showFileView.setToolTipText("Control+Shift+F");

      commands.showPeerView = new qx.ui.core.Command("Control+Shift+P");
      commands.showPeerView.setToolTipText("Control+Shift+P");

      this.__commands = commands;
    },

    wishSelectedFiles : function()
    {
      this.__filesTree.wishSelectedIds();
    },

    skipSelectedFiles : function()
    {
      this.__filesTree.skipSelectedIds();
    },

    unskipSelectedFiles : function()
    {
      this.__filesTree.unskipSelectedIds();
    },

    /**
     * TODOC
     *
     * @param e {Event} TODOC
     */
    syncStackView : function(e)
    {
      var selected = e.getData()[0];
      var view = selected != null ? selected.getUserData("value") : "";

      this.__activeView = view;

      // Are views initialized?
      if (this.__viewLoaded)
        this.selectView(view);
    },

    
    selectView : function(show)
    {
      var isFileViewEnabled = false;
      var isWishViewEnabled = false;
      this.info("Select view " + show);

      switch(show)
      {
        case "files":
          this.__stack.setSelection([ this.__filesView ]);
          this.__stack.show();
          this.__filesView.focus();
          isFileViewEnabled = true;
          break;

        case "wishlist":
          this.__stack.setSelection([ this.__wishesView ]);
          this.__stack.show();
          this.__wishesView.focus();
          isWishViewEnabled = true;
          break;

        case "peers":
//        this.__store.reloadPeers();
          this.__stack.setSelection([ this.__peersView ]);
          this.__stack.show();
          this.__peersView.focus();
          break;

        case "log":
          this.__stack.setSelection([ this.__logView ]);
          this.__stack.show();
          this.__logView.focus();
          break;

        default:
          this.__stack.resetSelection();
          this.__stack.exclude();
          this.__table.focus();
      }

      this.__filesTree.setActive(isFileViewEnabled);
      this.__wishesList.setActive(isWishViewEnabled);
    },


    /**
     * TODOC
     *
     * @param flag {Boolean} TODOC
     */
    setEnabled : function(isEnabled)
    {
      this.__toolBar.setEnabled(isEnabled);
      this.__table.setEnabled(isEnabled);
    },

    __setEnabled : function()
    {
      this.setEnabled(true);
    },

    __setDisabled : function()
    {
      this.setEnabled(false);
    }
  }
});
