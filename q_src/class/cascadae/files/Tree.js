/* ************************************************************************

#asset(cascadae/icon/16/files/folder-open-partical.png)
#asset(cascadae/icon/16/files/folder-open-skipped.png)
#asset(cascadae/icon/16/files/folder-partical.png)
#asset(cascadae/icon/16/files/folder-skipped.png)
#asset(cascadae/icon/16/files/office-skipped.png)

************************************************************************ */

qx.Class.define("cascadae.files.Tree",
{
  extend : qx.ui.treevirtual.TreeVirtual,

  events :
  {
    "activated"          : "qx.event.type.Event",
    "deactivated"        : "qx.event.type.Event",
    "d_childrenRequest"  : "qx.event.type.Data",
    "rd_childrenRespond" : "qx.event.type.Data",
    "rd_dataUpdated"     : "qx.event.type.Data",
    "d_wishFiles"        : "qx.event.type.Data",
    "d_skipFiles"        : "qx.event.type.Data",
    "d_unskipFiles"      : "qx.event.type.Data"
  },

  properties:
  {
    torrentId:
    {
      nullable:   true,
      init:       null,
      check:      "Number",
      apply:      "_applyTorrentId"
    }
  },

  
  construct : function()
  {
    var n2c =
    {
      "name"           : this.tr("Name"),
      "id"             : this.tr("Id"),
      "size"           : this.tr("Size"),
      "capacity"       : this.tr("Count"),
      "progress"       : this.tr("Progress"),
      "mode"           : this.tr("Mode")
    };
    var n2p = {};
    var ids = [];
    var captions = [];

    var i = 0;
    for (var id in n2c) {
        n2p[id] = i;
        ids[i] = id;
        captions[i] = n2c[id];
        i++;
    }
    delete i;
    
    var custom = {dataModel: new cascadae.files.Model()};
    this.base(arguments, captions, custom);
    // Highlighting of the focused rows is pretty slow.
    // Disable it.
    this.highlightFocusedRow(false);
    
    this.__n2p = n2p;
    this.__ids = ids;


    var tcm = this.getTableColumnModel();
    var rb = tcm.getBehavior();

    // Ensure that the tree column remains sufficiently wide
    rb.set(n2p.name,     { width:"1*", minWidth:180 });
    rb.set(n2p.size,     { width:"1*", minWidth:70, maxWidth:90  });
    rb.set(n2p.capacity, { width:"1*", minWidth:30, maxWidth:40  });
    rb.set(n2p.id,       { width:"1*", minWidth:30, maxWidth:40  });
    rb.set(n2p.progress, { width:"1*", minWidth:65, maxWidth:80  });

    [ n2p.capacity
    , n2p.id 
    , n2p.mode 
    ].map(function(id)
    {
      tcm.setColumnVisible(id, false);
    });

    tcm.setDataCellRenderer(n2p.size, new cascadae.cellrenderer.Size());
    tcm.setDataCellRenderer(n2p.progress, 
        new cascadae.cellrenderer.Progress());

    var sm = this.getSelectionModel();
    sm.setSelectionMode(qx.ui.table.selection.Model.MULTIPLE_INTERVAL_SELECTION);


    var dm = this.getDataModel();
    this.addListener("treeOpenWhileEmpty", this.onTreeOpenWhileEmpty, this);
    this.addListener("treeOpenWithContent", this.onTreeOpen, this);
    this.addListener("treeClose", this.onTreeClose, this);
    this.addListener("rd_childrenRespond", this.onDataLoad, this);
    this.addListener("rd_dataUpdated", this.onDataUpdated, this);
    
    this.setAlwaysShowOpenCloseSymbol(true);
    // delay preloading
    qx.event.Timer.once(this.__preloadImages, this, 3000);
  },

  members : {
    __n2p : [],
    __ids : [],
    __nid2sid : {},
    __sid2nid : {},
    __openSids : [],
    __dirSids : [],
    __active : false,
    // If true, than the data must be reloaded, when the table will be active
    // again.
    __dirty : false,

    _onKeyPress : function(e)
    {
      var code = e.getKeyIdentifier();
      if (code == "Escape") {
        var sm = this.getSelectionModel();
        sm.resetSelection();
      } else {
        this.base(arguments, e);
      }
    },
    

    addRows : function(tid, /*sid*/ parent_sid, rows)
    {
      var dm = this.getDataModel();
      var n2p = this.__n2p;
      var ids = this.__ids;

      var parent_nid = (parent_sid == 0) ? 0 : this.__sid2nid[parent_sid];


      for (var j = 0, nlen = rows.length; j<nlen; j++) {
        // extract node data array
        var row = rows[j];
        // get server id
        var sid = row[n2p.id];
        var name = row[n2p.name];
        var mode = row[n2p.mode];

        // add a node to the tree
        if (typeof(this.__sid2nid[sid]) == 'undefined') {
          if (this.__isLeaf(sid)) {
            var icon;
            switch(mode) {
                case "skip":
                    icon  = "cascadae/icon/16/files/office-skipped.png";
                    break;
                default:
                    icon  = "icon/16/mimetypes/office-document.png";
            }
            var nid = dm.addLeaf(parent_nid, name, icon);
          } else {
            var is_empty = row.capacity == 0;
            var is_open = -1 != this.__openSids[tid].indexOf(sid);
            var icon = this.__folderIcon(mode, is_open);
            var nid = dm.addBranch(parent_nid, name, is_open, is_empty, icon);
          }
        }
        // save indexes
        this.__nid2sid[nid] = sid;
        this.__sid2nid[sid] = nid;
        // set data for each data column
        for (var pos = 1, len = ids.length; pos<len; pos++) {
          dm.setColumnData(nid, pos, row[pos]);
        }
      }
    },

    __folderIcon : function(mode, is_open) {
        var icon;
        if (is_open)
        {
          switch(mode) {
            case "partial":
                icon = "cascadae/icon/16/files/folder-open-partical.png";
                break;
            case "skip":
                icon = "cascadae/icon/16/files/folder-open-skipped.png";
                break;
            default:
                icon = "icon/16/places/folder-open.png";
          }
        }
        else
        {
          switch(mode) {
            case "partial":
                icon = "cascadae/icon/16/files/folder-partical.png";
                break;
            case "skip":
                icon = "cascadae/icon/16/files/folder-skipped.png";
                break;
            default:
                icon = "icon/16/places/folder.png";
          }
        }
        return icon;
    },
    
    __preloadImages : function()
    {
      var il = qx.io.ImageLoader;

      var am = qx.util.AliasManager.getInstance();
      var rm = qx.util.ResourceManager.getInstance();

      var loadImage = function(f)
      {
        il.load(rm.toUri(am.resolve(f)));
      };

      loadImage("cascadae/icon/16/files/folder-open-partical.png");
      loadImage("cascadae/icon/16/files/folder-open-skipped.png");
      loadImage("cascadae/icon/16/files/folder-partical.png");
      loadImage("cascadae/icon/16/files/folder-skipped.png");
      loadImage("cascadae/icon/16/files/office-skipped.png");
      loadImage("icon/16/mimetypes/office-document.png");
      loadImage("icon/16/places/folder.png");
      loadImage("icon/16/places/folder-open.png");
    },


    __updateFolderIcon : function(node, invertOpened)
    {
      var n2p = this.__n2p;
      var dm = this.getDataModel();
      var mode = dm.getColumnData(node.nodeId, n2p.mode);
      var is_open = this.__xor(node.bOpened, invertOpened);
      node.icon = this.__folderIcon(mode, is_open);
    },

    __xor : function(x, y)
    {
      return !!(!x ^ !y);
    },

    __isLeaf : function(sid) {
      return -1 == this.__dirSids.indexOf(sid);
    },

    __clearState : function() {
      var tm = this.getDataModel();
      var sm = this.getSelectionModel();
      tm.clearData();
      sm.resetSelection();
      this.__nid2sid = {};
      this.__sid2nid = {};
      this.__dirSids = [];
    },


    setActive : function(bActive) {
      // is visable
      this.__active = bActive;
      if (bActive && this.__dirty) 
        this.updateData();
      this.__dirty = false;
      this.fireEvent(this.__active ? "activated" : "deactivated");
    },


    /**
     * Node was closed 
     */
    onTreeOpenWhileEmpty : function(e)
    {
      var node = e.getData();
      var tid = this.getTorrentId();
      var sid = this.__nid2sid[node.nodeId];
      this.getFileTreeNode(tid, [sid]);
      this.onTreeOpen(e);
    },

    getFileTreeNode: function(tid, sids)
    {
      this.fireDataEvent("d_childrenRequest", {torrent_id: tid, file_ids: sids});
    },

    onTreeOpen : function(e)
    {
      var node = e.getData();
      this.__updateFolderIcon(node, true);
      var tid = this.getTorrentId();
      var sid = this.__nid2sid[node.nodeId];
      this.__openSids[tid].push(sid);
      this.__openSids[tid] = this.__openSids[tid].sort();
    },


    /**
     * Node was opened 
     */
    onTreeClose : function(e)
    {
      var node = e.getData();
      this.__updateFolderIcon(node, true);
      var tid = this.getTorrentId();
      var sid = this.__nid2sid[node.nodeId];
      qx.lang.Array.remove(this.__openSids[tid], sid);
      this.__fireIconChanged(node);
    },


    /**
     * Data was loaded from the server
     */
    onDataLoad : function(e)
    {
      var data = e.getData();
      var tid = data.torrent_id;  
      if (this.getTorrentId() != tid)
        return;

      // Get table model
      var dm = this.getDataModel();

      // For each parent
      for (var plen = data.nodes.length, i = 0; i<plen; i++) 
      {
        // Relation is {parent_id, children}
        var rel = data.nodes[i];
        var rows = [];
        
        // For each child of the parent
        for (var j = 0, rlen = rel.children.length; j<rlen; j++) 
        {
          var child = rel.children[j];
          var sid = child.id;

          // save to the list of dirs
          if (child.is_leaf == false)
            this.__dirSids.push(sid);

          rows[j] = this.__mapToRow(child);
        }

        dm.sortRows(rows);

        // Add children to the model
        this.addRows(tid, rel.parent_id, rows); 
      }

      // update data in the table 
      dm.setData();
    },

    onDataUpdated : function(e)
    {
      var dm = this.getDataModel(),
          data = e.getData(),
          tid = data.torrent_id,
          n2p = this.__n2p;
      if (this.getTorrentId() != tid)
        return;
      for (var plen = data.nodes.length, i = 0; i<plen; i++) 
      {
        // node is {id, progress}
        var node = data.nodes[i];
        var nodeId  = this.__sid2nid[node.id];
        // row is not found.
        if (nodeId == undefined) continue;
        var rowIndex = dm.getRowFromNodeId(nodeId);
        // setValue(Integer columnIndex, Integer rowIndex, var value);
        if (node.progress !== undefined)
            dm.setValue(n2p.progress, rowIndex, node.progress);
      }
    },

    /* Map is from server */
    __mapToRow : function(map)
    {
      var row = [];
      var ids = this.__ids;

      for (var pos = 0, len = ids.length; pos<len; pos++) {
        var id = ids[pos];
        row[pos] = map[id];
      }
      return row;
    },


    getSelectedIds : function()
    {
      var nodes = this.getSelectedNodes();
      var sids = [];
      for (var i = 0, nlen = nodes.length; i < nlen; i++) {
        var nid = nodes[i].nodeId;
        sids[i] = this.__nid2sid[nid];
      }
      return sids;
    },

    wishSelectedIds : function()
    {
      // Files' identificators on the server
      var sids = this.getSelectedIds();
      var data = {torrent_id: this.getTorrentId(), file_ids: sids};
      this.fireDataEvent("d_wishFiles", data);
    },

    skipSelectedIds : function()
    {
      var self = this;
      // Files' identificators on the server
      var sids = this.getSelectedIds();
      var data = {torrent_id: this.getTorrentId(), file_ids: sids};
      this.fireDataEvent("d_skipFiles", data);

      // Update tree icons.
      var nodes = this.getSelectedNodes();
      var dm = this.getDataModel();
      var nodeArr = dm.getData();
      var n2p = this.__n2p;
      var mode_pos = n2p.mode;
      for (var i = 0; i < nodes.length; i++)
      {
        var node = nodes[i];
        // This node was skipped earlier.
        // Check the next node.
        if (node.columnData[mode_pos] == "skip")
          continue;
        // Update itself.
        node.columnData[mode_pos] = "skip";
        this.__setSkipIconForNode(node);
        // Update descenders.
        this.__setModeRec(node.children, "skip", mode_pos, nodeArr,
                          function(node) {self.__setSkipIconForNode(node);});
        // Update ancestors.
        while (node.parentNodeId)
        {
          var nid = node.parentNodeId;
          node = nodeArr[nid];
          var mode = node.columnData[mode_pos];
          // This node was skipped earlier.
          // Stop hierarchy traversing.
          if (mode == "skip")
            break; // while
          if (this.__haveAllSameValue(node.children, "skip", mode_pos, nodeArr))
          {
            // All children were skipped, skip their parent too.
            node.columnData[mode_pos] = "skip";
            node.iconSelected = node.icon = this.__folderIcon("skip", node.bOpened);
            continue;
          }
          // This node was partial earlier.
          // Stop hierarchy traversing.
          if (mode == "partial")
            break; // while
          node.columnData[mode_pos] = "partial";
          node.iconSelected = node.icon = this.__folderIcon("partial", node.bOpened);
        }
      }
      this.__fireIconChanged();
    },

    unskipSelectedIds : function()
    {
      // Files' identificators on the server
      var sids = this.getSelectedIds();
      var data = {torrent_id: this.getTorrentId(), file_ids: sids};
      this.fireDataEvent("d_unskipFiles", data);

      // Update tree icons.
      var nodes = this.getSelectedNodes();
      var dm = this.getDataModel();
      var nodeArr = dm.getData();
      var n2p = this.__n2p;
      var mode_pos = n2p.mode;
      for (var i = 0; i < nodes.length; i++)
      {
        var node = nodes[i];
        // This node was OK earlier.
        // Check the next node.
        if (node.columnData[mode_pos] == "download")
          continue;
        // Update itself.
        node.columnData[mode_pos] = "download";
        this.__setNormalIconForNode(node);
        // Update descenders.
        this.__setModeRec(node.children, "download", mode_pos, nodeArr,
                          function(node) {self.__setNormalIconForNode(node);});
        // Update ancestors.
        while (node.parentNodeId)
        {
          var nid = node.parentNodeId;
          node = nodeArr[nid];
          var mode = node.columnData[mode_pos];
          // This node was marked as wanted earlier.
          // Stop hierarchy traversing.
          if (mode == "download")
            break; // while
          if (this.__haveAllSameValue(node.children, "download", mode_pos, nodeArr))
          {
            node.columnData[mode_pos] = "download";
            node.iconSelected = node.icon = this.__folderIcon("download", node.bOpened);
            continue;
          }
          // This node was partial earlier.
          // Stop hierarchy traversing.
          if (mode == "partial")
            break; // while
          node.columnData[mode_pos] = "partial";
          node.iconSelected = node.icon = this.__folderIcon("partial", node.bOpened);
        }
      }
      this.__fireIconChanged();
    },


    __fireIconChanged : function(node)
    {
      var data;
      var dm = this.getDataModel();
      if (node === undefined)
      {
          data =
          {
            firstRow    : 0,
            lastRow     : dm.getRowCount() - 1,
            firstColumn : 0,
            lastColumn  : 0
          };
      }
      else
      {
          var rowId = dm.getRowFromNodeId(node.nodeId);
          data =
          {
            firstRow    : rowId,
            lastRow     : rowId,
            firstColumn : 0,
            lastColumn  : 0
          };
      }
      dm.fireDataEvent("dataChanged", data);
    },

    __setSkipIconForNode : function(node)
    {
      if (node.type == qx.ui.treevirtual.MTreePrimitive.Type.LEAF)
      {
        node.iconSelected = node.icon = "cascadae/icon/16/files/office-skipped.png";
      } else {
        node.iconSelected = node.icon = this.__folderIcon("skip", node.bOpened);
      }
    },

    __setNormalIconForNode : function(node)
    {
      if (node.type == qx.ui.treevirtual.MTreePrimitive.Type.LEAF)
      {
        node.iconSelected = node.icon = "icon/16/mimetypes/office-document.png";
      } else {
        node.iconSelected = node.icon = this.__folderIcon("download", node.bOpened);
      }
    },

    __haveAllSameValue: function(nids, value, column_pos, nodeArr)
    {
      for (var i = 0; i < nids.length; i++)
      {
        if (nodeArr[ nids[i] ].columnData[column_pos] != value)
          return false;
      }
      return true;
    },

    __setModeRec: function(children, mode, mode_pos, nodeArr, iter)
    {
      for (var i = 0; i < children.length; i++)
      {
        var nid = children[i],
            node = nodeArr[nid];
        if (!node)
          break; // not loaded yet.
        if (node.columnData[mode_pos] == mode)
          continue;
        node.columnData[mode_pos] = mode;
        iter(node);
        this.__setModeRec(node.children, mode, mode_pos, nodeArr);
      }
    },

    // rows is a map from sid to columndata.
    // protected -- called from the model.
    //
    // psid2rows will be changed in this function.
    setRowsBulk : function(psid2rows)
    {
      var tid = this.getTorrentId(),
           dm = this.getDataModel();
      var dirSids = this.__dirSids;
      this.__clearState();
      this.__dirSids = dirSids;
      // this.__openSids[tid] is the same.

      for (var parent_sid in psid2rows)
      {
        var rows = psid2rows[parent_sid];
        dm.sortRows(rows);

        // Add children to the model
        this.addRows(tid, parent_sid, rows); 
      }

      // update data in the table 
      dm.setData();
    },

    _applyTorrentId: function(value, old, name)
    {
      if (this.__active) this.updateData();
      else this.__dirty = true;
    },

    updateData : function()
    {
      this.__clearState();
      var tid = this.getTorrentId();
      if (typeof(this.__openSids[tid]) == "undefined") {
        this.__openSids[tid] = [0];
      }
      if (tid !== null) this.getFileTreeNode(tid, this.__openSids[tid]);
    },

    getNodeToServerIdIndex: function()
    {
        return this.__nid2sid;
    }
  }
});
