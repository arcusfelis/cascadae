
/**
 * The string data cell renderer. All it does is escape the incoming String
 * values.
 */
qx.Class.define("cascadae.cellrenderer.Speed",
{
  extend : qx.ui.table.cellrenderer.Conditional,

  members :
  {
    // overridden
    /**
     * TODOC
     *
     * @param cellInfo {var} TODOC
     * @return {var} TODOC
     */
    _getContentHtml : function(cellInfo) {
      return cascadae.Helpers.bytesToSize(cellInfo.rowData[cellInfo.col], 0);
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
