
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("cascadae.Table",
{
  extend : cascadae.BasicTable,

  properties:
  {
    torrentId:
    {
      nullable:   true,
      init:       null,
      check:      "Number",
      event:      "changeTorrentId"
    }
  },

  events :
  {
    "d_startTorrents" : "qx.event.type.Data",
    "d_stopTorrents"  : "qx.event.type.Data"
  },

  construct : function(speedInfo)
  {
    this.__speedInfo = speedInfo;

    var n2c =
    {
      "id"                  : this.tr("Id"),
      "pid"                 : this.tr("Pid"),
      "name"                : this.tr("Torrent Name"),
      "display_name"        : this.tr("Name"),
      "info_hash"           : this.tr("Info Hash"),
      "wanted"              : this.tr("Wanted"),
      "total"               : this.tr("Total"),
      "left"                : this.tr("Left"),
      "progress"            : this.tr("Progress"),
      "rating"              : this.tr("Rating"),
      "online"              : this.tr("On-line"),
      "seeders"             : this.tr("Ss"),
      "leechers"            : this.tr("Ls"),
      "state"               : this.tr("State"),
      "downloaded"          : this.tr("In Now"),
      "uploaded"            : this.tr("Out Now"),
      "all_time_downloaded" : this.tr("In Before"),
      "all_time_uploaded"   : this.tr("Out Before"),
      "sum_downloaded"      : this.tr("Total In"),
      "sum_uploaded"        : this.tr("Total Out"),
      "speed_in"            : this.tr("Speed In"),
      "speed_out"           : this.tr("Speed Out")
    };

    this.base(arguments, n2c);

    var tm = this.getTableModel();
    var tcm = this.getTableColumnModel();
    var n2p = this.getColumnNameToPositionIndex();

    var rb = tcm.getBehavior();

    rb.set(n2p.name,        { width:"1*", minWidth: 250 });
    rb.set(n2p.display_name, { width:"1*", minWidth: 250 });
    rb.set(n2p.info_hash, { width:"1*", minWidth: 250 });
    rb.set(n2p.id,        { width:"1*", minWidth: 30 });
    rb.set(n2p.pid,       { width:"1*", minWidth: 70 });
    rb.set(n2p.progress,  { width:"1*", minWidth: 65 });
    rb.set(n2p.total,     { width:"1*", minWidth: 70 });
    rb.set(n2p.wanted,    { width:"1*", minWidth: 70 });
    rb.set(n2p.left,      { width:"1*", minWidth: 70 });
    rb.set(n2p.leechers,  { width:"1*", minWidth: 40 });
    rb.set(n2p.seeders,   { width:"1*", minWidth: 40 });
    rb.set(n2p.state,     { width:"1*", minWidth: 70 });
    rb.set(n2p.rating,    { width:"1*", minWidth: 60 });
    rb.set(n2p.downloaded,{ width:"1*", minWidth: 70 });
    rb.set(n2p.uploaded,  { width:"1*", minWidth: 70 });
    rb.set(n2p.speed_in,  { width:"1*", minWidth: 70 });
    rb.set(n2p.speed_out, { width:"1*", minWidth: 70 });
    rb.set(n2p.all_time_downloaded, { width:"1*", minWidth: 65 });
    rb.set(n2p.all_time_uploaded,   { width:"1*", minWidth: 70 });
    rb.set(n2p.sum_downloaded,      { width:"1*", minWidth: 70 });
    rb.set(n2p.sum_uploaded,        { width:"1*", minWidth: 70 });
    rb.set(n2p.online,    { width:"1*", minWidth: 30 });

    tcm.setDataCellRenderer(n2p.progress, 
      new cascadae.cellrenderer.Progress());
    tcm.setDataCellRenderer(n2p.rating, new cascadae.cellrenderer.Rating());
    tcm.setDataCellRenderer(n2p.online, new qx.ui.table.cellrenderer.Boolean());

    [ n2p.left
    , n2p.wanted
    , n2p.downloaded
    , n2p.uploaded
    , n2p.all_time_downloaded
    , n2p.all_time_uploaded
    , n2p.sum_downloaded
    , n2p.sum_uploaded ].map(function(id) {
      tcm.setDataCellRenderer(id, new cascadae.cellrenderer.Size());
    });

    tcm.setDataCellRenderer(n2p.speed_in, new cascadae.cellrenderer.Speed());
    tcm.setDataCellRenderer(n2p.speed_out, new cascadae.cellrenderer.Speed());

    // Hide columns
    [ n2p.name
    , n2p.info_hash
    , n2p.left
    , n2p.total
    , n2p.online
    , n2p.downloaded
    , n2p.uploaded
    , n2p.all_time_downloaded
    , n2p.all_time_uploaded ].map(function(id)
    {
      tcm.setColumnVisible(id, false);
    });

    tm.setSortMethods(n2p.pid, cascadae.Helpers.buildPidComparator(n2p.pid));
    var sm = this.getSelectionModel();
    sm.addListener("changeSelection", this.__refreshTorrentId, this);
  },

  members :
  {

    /**
     * TODOC
     *
     * @param rowData {var} TODOC
     * @return {var} TODOC
     */
    __calcProgress : function(rowData)
    {
      var n2p = this.getColumnNameToPositionIndex();

      var wanted = rowData[n2p.wanted];
      var left = rowData[n2p.left];
      var completed = wanted - left;

      return (completed / wanted);
    },


    /**
     * TODOC
     *
     * @param rowData {var} TODOC
     * @return {int | var} TODOC
     */
    __calcRating : function(rowData)
    {
      var n2p = this.getColumnNameToPositionIndex();

      var nowU = rowData[n2p.uploaded];
      var nowD = rowData[n2p.downloaded];
      var beforeU = rowData[n2p.all_time_uploaded];
      var beforeD = rowData[n2p.all_time_downloaded];

      var totalD = nowD + beforeD;
      var totalU = nowU + beforeU;
      if (totalD == 0) return 0;
      return (totalU / totalD).toFixed(2);
    },


    /**
     * TODOC
     *
     * @param rowData {var} TODOC
     * @return {var} TODOC
     */
    __calcSumDownloading : function(rowData)
    {
      var n2p = this.getColumnNameToPositionIndex();

      var now = rowData[n2p.downloaded];
      var before = rowData[n2p.all_time_downloaded];

      return now + before;
    },


    /**
     * TODOC
     *
     * @param rowData {var} TODOC
     * @return {var} TODOC
     */
    __calcSumUploading : function(rowData)
    {
      var n2p = this.getColumnNameToPositionIndex();

      var now = rowData[n2p.uploaded];
      var before = rowData[n2p.all_time_uploaded];

      return now + before;
    },


    /**
     * TODOC
     *
     */
    startSelectedRows : function()
    {
      this.info("startSelectedRows");
      var data = {"torrent_ids": this.getSelectedIds()};
      this.fireDataEvent("d_startTorrents", data);
    },


    /**
     * TODOC
     *
     */
    stopSelectedRows : function()
    {
      this.info("stopSelectedRows");
      var data = {"torrent_ids": this.getSelectedIds()};
      this.fireDataEvent("d_stopTorrents", data);
    },


    /**
     * Set data from row (map from server) to newValues (row in the table).
     *
     * @param row {var} TODOC
     * @param newValues {var} TODOC
     * @param add {var} TODOC
     * @return {var} TODOC
     */
    fillFields : function(row, newValues, add)
    {
      newValues = this.base(arguments, row, newValues, add);
      var n2p = this.getColumnNameToPositionIndex();

      if (add)
      {
        newValues[n2p.speed_in] = 0;
        newValues[n2p.speed_out] = 0;
      }

      newValues[n2p.sum_uploaded] = this.__calcSumUploading(newValues);
      newValues[n2p.sum_downloaded] = this.__calcSumDownloading(newValues);
      newValues[n2p.rating] = this.__calcRating(newValues);
      newValues[n2p.progress] = this.__calcProgress(newValues);

      return newValues;
    },

    /**
     * Handles updates from the store.
     */
    logHandler: function(e)
    {
      var data = e.getData();
      var name = data.name;
      if (name == "started_torrent") {
        this.setTorrentOnline(data.torrent_id, true);
      } 
      else 
      if (name == "stopped_torrent") {
        this.setTorrentOnline(data.torrent_id, false);
      }
    },

    setTorrentOnline: function(torrentId, isOnline) 
    {
      this.particallyUpdateRows([{"id" : torrentId, "online" : isOnline}]);
    },

    __refreshTorrentId: function()
    {
      var ids = this.getSelectedIds();
      var torrent_id =  ids.length ? ids[0] : null;
      this.setTorrentId(torrent_id);
    },

    _updateStatusBar : function()
    {
      var tableModel = this.getTableModel();
      var box = new qx.ui.layout.HBox();

      if (this.getStatusBarVisible())
      {
        var selectedRowCount = this.getSelectionModel().getSelectedCount();
        var rowCount = tableModel.getRowCount();

        var text;

        if (rowCount >= 0)
        {
          if (selectedRowCount == 0) {
            text = this.trn("one row", "%1 rows", rowCount, rowCount);
          } else {
            text = this.trn("one of one row", "%1 of %2 rows", rowCount, selectedRowCount, rowCount);
          }
        }

        if (text && this.__statusbarInfo) {
          this.__statusbarInfo.setValue(text);
        }
      }
    },
    
    _createChildControlImpl : function(id, hash)
    {
      var control;

      switch(id)
      {
      case "statusbar":
        control = new qx.ui.container.Composite(new qx.ui.layout.HBox());
        control.set({ allowGrowX: true });
        this.__statusbarInfo = new qx.ui.basic.Label();
        this.__speedInfo.addListener("mousedown", this.__dispaySpeedControl, this);
        control.add(this.__statusbarInfo);
        control.add(new qx.ui.core.Spacer, {flex: 1});
        control.add(this.__speedInfo);
        this._add(control);
      }
      return control || this.base(arguments, id);
    },

    __speedControlPane : null,
    __dispaySpeedControl: function()
    {
      if (!this.__speedControlPane)
      {
        this.__speedControlPane = new cascadae.speedControl.Pane(this.__speedInfo);
        this.__speedControlPane.addListener("changeVisibility",
                this.__onSpeedControlPaneVisibilityChange, this);
      }
      var pane = this.__speedControlPane;
      if (pane.isVisible())
      {
        pane.hide();
      } else {
        pane.placeToWidget(this.__speedInfo);
        pane.show();
      }
    },
    // A mousedown event on any widget under the popup will close it.
    // A mousedown  eventon the button will open it again, even if it was closed
    // a millisecond ago.
    // To disable this behaviour we use this handler.
    __onSpeedControlPaneVisibilityChange: function(e)
    {
      if (this.__speedControlPane.isVisible())
        this.__speedInfo.removeListener("mousedown", this.__dispaySpeedControl, this);
      else 
        this.__speedInfo.addListener("mousedown", this.__dispaySpeedControl, this);
    }
  }
});
