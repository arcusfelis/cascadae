
/**
 * The string data cell renderer. All it does is escape the incoming String
 * values.
 */
qx.Class.define("cascadae.cellrenderer.Progress",
{
  extend : qx.ui.table.cellrenderer.Conditional,

  members :
  {
    __calcFn : null,

    // overridden
    /**
     * TODOC
     *
     * @param cellInfo {var} TODOC
     * @return {var} TODOC
     */
    _getContentHtml : function(cellInfo) {
      return  (cellInfo.rowData[cellInfo.col] * 100).toFixed(2) + "%";
    },

    // overridden
    /**
     * TODOC
     *
     * @param cellInfo {var} TODOC
     * @return {string} TODOC
     */
    _getCellClass : function(cellInfo) {
      return "qooxdoo-table-cell-right qooxdoo-table-cell";
    }
  }
});
