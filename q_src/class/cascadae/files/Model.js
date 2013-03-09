qx.Class.define("cascadae.files.Model",
{
  extend : qx.ui.treevirtual.SimpleTreeDataModel,
  
  construct : function(table)
  {
    this.__table = table;
    this.base(arguments);
  },

  members : {
    __table : null,
    __columnIndex : -1,
    __asc : true,

    isColumnSortable : function(columnIndex) {
      return true;
    },

    sortByColumn : function(columnIndex, ascending) {
      this.__columnIndex = columnIndex;
      this.__asc = ascending;
      this.__table.sortHandler();
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
