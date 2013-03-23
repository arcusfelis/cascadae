
/**
 * The string data cell renderer. All it does is escape the incoming String
 * values.
 */
qx.Class.define("cascadae.cellrenderer.Size",
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
      return this.__bytesToSize(cellInfo.rowData[cellInfo.col], 2);
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
    },


    /**
     * Convert number of bytes into human readable format
     *
     * @param bytes {var} TODOC
     * @param precision {var} TODOC
     * @return {var} string
     */
    __bytesToSize : function(bytes, precision)
    {
      var kilobyte = 1024;
      var megabyte = 1048576;
      var gigabyte = 1073741824;

      if (bytes < kilobyte) {
        return bytes + ' B';
      } else if (/* (bytes >= kilobyte) && */ (bytes < megabyte)) {
        return (bytes / kilobyte).toFixed(precision) + ' KB';
      } else if (/* (bytes >= megabyte) && */ (bytes < gigabyte)) {
        return (bytes / megabyte).toFixed(precision) + ' MB';
      } else /* if ( (bytes >= gigabyte)) */ {
        return (bytes / gigabyte).toFixed(precision) + ' GB';
      }
    }
  }
});
