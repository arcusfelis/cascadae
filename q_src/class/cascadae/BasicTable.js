
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("cascadae.BasicTable",
{
  extend : qx.ui.table.Table,


  /**
   * @lint ignoreUndefined(qxc)
   */
  construct : function(names)
  {
    // INIT TABLE MODEL
    // table model
    this.__tableModel = new smart.model.Default;

    // SET COLUMN NAMES (do it before running the base constructor)
    this.__columnsNameToCaption = names;

    this.__columnNums = [];
    this.__columnNames = [];
    this.__columnCaptions = [];

    var i = 0;

    for (var name in this.__columnsNameToCaption)
    {
      var caption = this.__columnsNameToCaption[name];
      this.__columnNums[name] = i;
      this.__columnNames[i] = name;
      this.__columnCaptions[i] = caption;
      i++;
    }

    delete i;

    this.__tableModel.setColumns(this.__columnCaptions, this.__columnNames);

    // Install tableModel
    this.base(arguments, this.__tableModel, {
        tableColumnModel : function(obj) {
          return new qx.ui.table.columnmodel.Resize(obj);
        }
    });

    // INIT SELECTION MODEL
    var sm = this.__selectionModel = this.getSelectionModel();
    sm.setSelectionMode(qx.ui.table.selection.Model.MULTIPLE_INTERVAL_SELECTION);

    var n2p = this.getColumnNameToPositionIndex();

    var tm = this.__tableModel;

    // Add the index to SmartTableModel
    tm.indexedSelection(n2p.id, sm);
    tm.addIndex(n2p.id);

    // Set handlers for remote events.
    var eventHandlers =
    {
      "rd_dataAdded"         : this.__onDataAdded,
      "rd_dataRemoved"       : this.__onDataRemoved,
      "rd_dataRemoveFailure" : this.__onDataRemoveFailure,
      "rd_dataLoadCompleted" : this.__onDataLoadCompleted,
      "rd_dataUpdated"       : this.__onDataUpdated
    };

    for (var eventName in eventHandlers)
    {
      this.addListener(eventName, eventHandlers[eventName], this);
    }
    this.setAlwaysUpdateCells(false);
  },

  members :
  {
    __tableModel : null,
    __selectionModel : null,
    __columnNums : null,
    __columnNames : null,
    __columnCaptions : null,
    __active : false,
    // Is this widget newer activated?
    __virgin : true,
    // If true, than the data must be reloaded, when the table will be active
    // again.
    __dirty : false,

    _onKeyPress : function(e)
    {
      var code = e.getKeyIdentifier();
      if (code == "Escape") {
        this.__selectionModel.resetSelection();
      } else {
        this.base(arguments, e);
      }
    },


    /**
     * INITIALIZATION PART
     *
     * @param names {var} TODOC
     */
    setColumnNames : function(names) {},


    /**
     * HELPERS
     *
     * @return {var} TODOC
     */
    getColumnNameToPositionIndex : function() {
      return this.__columnNums;
    },


    /**
     * TODOC
     *
     */
    logSelectedRows : function()
    {
      var tm = this.__tableModel;

      this.__selectionModel.iterateSelection(function(pos)
      {
        console.log("Position " + pos);
        console.dir(tm.getRowData(pos));
      });
    },


    /**
     * Returns an array of object's ids.
     *
     * @return {var} TODOC
     */
    getSelectedIds : function()
    {
      var ids = [];
      var tm = this.__tableModel;
      var n2p = this.getColumnNameToPositionIndex();

      this.__selectionModel.iterateSelection(function(pos) {
        ids.push(tm.getValue(n2p.id, pos));
      });

      return ids;
    },


    __rowQueue : undefined,

    /**
     * DATA MANAGEMENT
     *
     * @param Rows {var} TODOC
     */
    particallyUpdateRows : function(Rows)
    {
      if (!this.__rowQueue) 
      {
        this.__rowQueue = Rows;
        qx.event.Timer.once(this.__nonBlockingUpdate, this, 1);
      } else {
        var idx = {};
        for (var i = 0, l = this.__rowQueue.length; i < l; i++)
        {
            var row = this.__rowQueue[i];
            idx[row.id] = i;
        }
            
        for (var i = 0, l = Rows.length; i < l; i++)
        {
          var row = Rows[i];
          var oldRowPos = idx[row.id];
          if (oldRowPos !== undefined)
          {
            // previous update is still in queue
            var oldRow = this.__rowQueue[oldRowPos];
            qx.lang.Object.mergeWith(oldRow, row);
          } else {
            this.__rowQueue.push(Rows[i]);
          }
        }
      }
    },

    __nonBlockingUpdate : function()
    {
      var tm = this.__tableModel,
          n2p = this.getColumnNameToPositionIndex();

      tm.saveSelection();
      console.log("non blocking update start");
      for (var i = 0, l = Math.min(10, this.__rowQueue.length); i < l; i++)
      {
        var row = this.__rowQueue.shift();
        var pos = tm.locate(n2p.id, row.id);
        if (isNaN(pos))
        {
          this.error("Cannot locate row.");
          continue;
        }
        // Update whole row.
        var oldValues = tm.getRowData(pos,
                           /* view */ undefined,
                           /* copy */ false);
        if (typeof (oldValues) != "object")
        {
          this.error("Cannot retrieve old values.");
          continue;
        }
        var newValues = qx.lang.Array.clone(oldValues);
        newValues = this.fillFields(row, newValues, false);
        
        tm.setRow(/* rowIndex */ pos,
                  newValues,
                  /* view */ undefined,
                  /* fireEvent */ false,
                  /* preserveSelection */ false);
      }
      console.log("non blocking update stop");
      tm.restoreSelection();
      tm.forceRedraw();
      console.log("non blocking update end");

      if (this.__rowQueue.length)
      {
        qx.event.Timer.once(this.__nonBlockingUpdate, this, 4);
        console.log("Len " + this.__rowQueue.length);
      } else {
        this.__rowQueue = undefined;
        this.fireEvent("tableRefreshed");
      }
    },

    /* The event is from the user. */

    /**
     * TODOC
     *
     */
    removeSelectedRows : function()
    {
      this.info("RemoveSelectedRows");

      this.logSelectedRows();
      var table = this;

      // Extract ids, because iterator is not nice.
      var ids = this.getSelectedIds();

      this.__removeIds(ids);
    },


    /**
     * TODOC
     *
     * @param data {var} TODOC
     */
    addRows : function(data)
    {
      var rows = [];

      for (var i in data)
      {
        var row = data[i];
        rows[i] = this.fillFields(row, [], true);
      }

      this.debug("Bulk update.");

      this.__tableModel.addRows(rows, /* copy */ true, /* fireEvent */ false);

      this.updateContent();
      this.getPaneScroller(0).updateVerScrollBarMaximum();
      this._updateScrollBarVisibility();
      this.fireEvent("tableRefreshed");
    },


    /**
     * TODOC
     *
     * @param ids {var} TODOC
     */
    __removeIds : function(ids)
    {
      var n2p = this.getColumnNameToPositionIndex();
      for (var i in ids)
      {
        this.info("Purge from the table entry by real id " + id);
        var id = ids[i];
        var pos = this.__tableModel.locate(n2p.id, id);

        /* Purge data from the table. */

        if (pos != 'undefined') this.__tableModel.removeRows(pos, 1);
      }
      this.fireEvent("tableRefreshed");
    },


    /**
     * Set data from row (map from server) to newValues (row in the table).
     *
     * @param row {var} TODOC
     * @param newValues {var} TODOC
     * @param add {var} TODOC
     * @return {var} TODOC
     */
    fillFields : function(row, newValues, add)  /* boolean : add or update */
    {
      var n2p = this.getColumnNameToPositionIndex();

      for (var j in row)
      {
        var cid = n2p[j];
        newValues[cid] = row[j];
      }

      return newValues;
    },


    /**
     * TODOC
     *
     * @param event {var} TODOC
     */
    __onDataLoadCompleted : function( /* qx.event.type.Data */ event)
    {
      var data = event.getData();

      try
      {
        this.__tableModel.removeRows(
        /* startIndex */ 0,
        /* howMany */ undefined,
        /* view */ undefined,
        /* fireEvent */ false);
      }
      catch(err)
      {
        this.debug("Is the table empty?");
      }

      if (data.rows.length) this.addRows(data.rows);
    },


    /**
     * TODOC
     *
     * @param event {var} TODOC
     */
    __onDataAdded : function( /* qx.event.type.Data */ event)
    {
      var data = event.getData();
      this.addRows(data.rows);
    },


    /**
     * The event is from the server. 
     *
     * @param event {qx.event.type.Data} TODOC
     */
    __onDataRemoved : function(event)
    {
      var ids = event.getData().rows;
      this.__removeIds(ids);
    },


    /**
     * TODOC
     *
     * @param event {qx.event.type.Data} TODOC
     */
    __onDataRemoveFailure: function(event)
    {
      var data = event.getOldData();
      this.__tableModel.addRowsAsMapArray([data]);
      this.fireEvent("tableRefreshed");
    },


    /**
     * TODOC
     *
     * @param event {qx.event.type.Data} TODOC
     */
    __onDataUpdated : function(event)
    {
      var data = event.getData();
      this.particallyUpdateRows(data.rows);
    },

    _oldSelection : [],
    __unfilteredView : undefined,
    __filteredView : undefined,
    __mainTable : undefined,

    /**
     * TODOC
     *
     * @param main {var} Table with torrents
     */
    initFilters : function(main)
    {
      if (!main) throw("Specify the main table");
      this.__mainTable = main;
      var tm = this.getTableModel();

      this.__unfilteredView = tm.getView();

      this.__filteredView = tm.addView(this._mainTableFilter, this);

      var msm = main.getSelectionModel();
      msm.addListener("changeSelection", this._changeMainTableSelection, this);
    },

    _mainTableFilter : function(row)
    {
      var n2p = this.getColumnNameToPositionIndex();
      var tid = row[n2p.torrent_id];
      return (this._oldSelection.indexOf(tid) != -1);
    },

    setActive : function(bActive) {
      if (bActive == this.__active)
          return;

      // is visable
      this.__active = bActive;
      if (bActive && this.__dirty) 
        this.updateFilters();
      this.__dirty = false;
      this.fireEvent(this.__active ? "activated" : "deactivated");

      this.info("Activate peer table " + this.__active);
      if (this.__virgin)
      {
        this.fireDataEvent("d_updateFilters",
                           {"torrent_ids": this._oldSelection});
        this.__virgin = false;
      }
    },

    _changeMainTableSelection: function(e) {
      if (this.__active) this.updateFilters();
      else this.__dirty = true;
    },

    /**
     * TODOC
     *
     */
    updateFilters : function() 
    {
      this.info("change selection");
      var tm = this.__tableModel; 

      var newSel = this.__mainTable.getSelectedIds();
      if (qx.lang.Array.equals(this._oldSelection, newSel)) return;
      this.fireDataEvent("d_updateFilters", {"torrent_ids": newSel});

      this._oldSelection = qx.lang.Array.clone(newSel);

      if (newSel.length == 0) {
        tm.setView(this.__unfilteredView);
      }
      else
      {
        if (tm.getView() != this.__filteredView) 
          tm.setView(this.__filteredView);
        tm.updateView(this.__filteredView);
      }
      this.fireEvent("tableRefreshed");
    }
  },

  events :
  {
    /**
     * The data is a map containing this properties:
     * <ul>
     *   <li>torrent_ids - New visible torrent ids</li>
     * </ul>
     */
    "d_updateFilters"      : "qx.event.type.Data",
    "activated"            : "qx.event.type.Event",
    "deactivated"          : "qx.event.type.Event",

    // Data in the table model was changed
    "tableRefreshed"       : "qx.event.type.Event",

    // Remote events
    "rd_dataAdded"         : "qx.event.type.Data",

    /**
     * Dispatched after a respond is trasmitted from server.
     *
     * The data is a map containing this properties:
     * <ul>
     *   <li>rows (of ids)</li>
     * </ul>
     */
    "rd_dataRemoved"       : "qx.event.type.Data",

    /**
     * Dispatched after a respond is trasmitted from server.
     *
     * The data is a map containing this properties:
     * <ul>
     *   <li>id</li>
     * </ul>
     */
    "rd_dataRemoveFailure" : "qx.event.type.Data",

    /**
     * Load a full set of data from server.
     *
     * The data is a map containing this properties:
     * <ul>
     *   <li>rows</li>
     * </ul>
     */
    "rd_dataLoadCompleted" : "qx.event.type.Data",

    /**
     * Dispatched after a respond is trasmitted from server.
     *
     * The data is a map containing this properties:
     * <ul>
     *   <li>rows : [{id, ...}]</li>
     * </ul>
     */
    "rd_dataUpdated"       : "qx.event.type.Data"
  }
});
