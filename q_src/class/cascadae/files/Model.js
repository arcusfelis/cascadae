qx.Class.define("cascadae.files.Model",
{
  extend : qx.ui.treevirtual.SimpleTreeDataModel,
  
  construct : function()
  {
    this.base(arguments);
  },

  members : {
    __columnIndex : -1,
    __asc : true,

    isColumnSortable : function(columnIndex) {
      return true;
    },

    sortByColumn : function(columnIndex, ascending) {
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
      this.fireEvent("metaDataChanged");
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
    }
  }
});
