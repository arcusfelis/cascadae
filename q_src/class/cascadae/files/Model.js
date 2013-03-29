qx.Class.define("cascadae.files.Model",
{
  extend : qx.ui.treevirtual.SimpleTreeDataModel,
  
  construct : function()
  {
    this.base(arguments);
  },

  members : {
    __columnIndex : -1,
    __keyColumn : 1,
    __asc : true,

    isColumnSortable : function(columnIndex) {
      return true;
    },

    sortByColumn : function(columnIndex, ascending) {
//    console.dir(this._rowArr);
//    console.dir(this._nodeArr);
      this.__columnIndex = columnIndex;
      this.__asc = ascending;

      var data = this.getData();
      var tree = this.getTree(); // cascadae.files.Tree
      var rows = {};
      var n2s  = tree.getNodeToServerIdIndex();
      for (var i = 1; i < data.length; i++)
      {
        var node = data[i];
        var sid = node.parentNodeId ? n2s[node.parentNodeId] : 0;
        if (rows[sid] == undefined)
            rows[sid] = [];
        var row = node.columnData;
        row[0] = node.label;
        rows[sid].push(row);
      }
      tree.setRowsBulk(rows);
      this.fireEvent("metaDataChanged");
//    this.forceRedraw();
    },

    forceRedraw : function()
    {
      if (this.hasListener("dataChanged"))
      {
        var data =
        {
          firstRow    : 0,
          lastRow     : this._rowArr.length - 1,
          firstColumn : 0,
          lastColumn  : this.getColumnCount() - 1
        };

        this.fireDataEvent("dataChanged", data);
      }
    },


    getSortColumnIndex : function() {
      return this.__columnIndex;
    },

    isSortAscending : function() {
      return this.__asc;
    },

    sortCompleted : function() {
      var data =
        {
          columnIndex : this.__columnIndex,
          ascending   : this.__asc
        };
      this.fireDataEvent("sorted", data);
    },

    getRowId : function(row) {
        return row[this.__keyColumn];
    },

    getRowVersion : function(row, colNums) {
        var version = "";
        // Called from the tree pane
        if (~colNums.indexOf(0))
            version += row[0].bOpened + row[0].icon;

        // Called from the metadata pane
        if (~colNums.indexOf(4))
          version += row[4]; // progress
        if (~colNums.indexOf(5))
          version += row[5]; // mode

        return version;
    },

    sortRows : function(rows) {
      if (this.__columnIndex == -1)
        return;

      var comparator =
        (this.__asc
         ? qx.ui.table.model.Simple._defaultSortComparatorInsensitiveAscending
         : qx.ui.table.model.Simple._defaultSortComparatorInsensitiveDescending);

      comparator.columnIndex = this.__columnIndex;
      rows.sort(comparator);
    },

    __selectionIndex : undefined,
    __selectionModel : undefined,
    __selectedRows : undefined,

    indexedSelection: function(columnIndex, selectionModel)
    {
      this.__selectionIndex = columnIndex;
      this.__selectionModel = selectionModel;
      this.__selectedRows = [];
    },

    // Save the list of indices corresponding to the set of selected rows
    // (push).
    getSelection: function()
    {
      if (!this.__selectionModel ||
          this.__selectionIndex < 0 ||
          this.__selectionIndex >= this.getColumnCount())
      {
        return;
      }

      // Initialize the array containing the selected rows.
      var selected = [];

      // Iterate through the selected rows. For each selected row...
      this.__selectionModel.iterateSelection(
        function(row)
        {
          // Get the value from the indexed column, and push it to the
          // selected rows array.
          selected.push(this.getValue(this.__selectionIndex, row));
        },
        this);
      return selected;
    },


    setSelection: function(selected)
    {
      // If there's no indexed selection, there's nothing to restore.
      if (! this.__selectionModel ||
          this.__selectionIndex < 0 ||
          this.__selectionIndex >= this.getColumnCount())
      {
        return;
      }

      // Get a local reference ot the selection model
      var sm = this.__selectionModel;
      
      // Queue events for selection changes
      sm.setBatchMode(true);
      
      // ... then clear the current selection
      this.__clearSelection();

      // For each selected row...
      for (var i = 0; i < selected.length; i++)
      {
        // Find that row
        var row = this.find(this.__selectionIndex, selected[i]);
        
        // If it was found...
        if (row !== undefined)
        {
          // ... then add it to the selection in the selection model
//        console.log("Add selection interval " +row);
          sm.addSelectionInterval(row, row);
        }
      }
      
      // Send selection events now.
      sm.setBatchMode(false);
    },

    find: function(columnIndex, value)
    {
      var rows = this._rowArr;
      for (var i = 0, l = rows.length; i < l; i++)
        if (rows[i][columnIndex] === value)
          return i;
      return undefined;
    },

    //
    // Clear the selection.
    //
    __clearSelection: function()
    {
      var sm = this.__selectionModel;
      if (sm)
      {
        sm.resetSelection();
      }
    }
  }
});
