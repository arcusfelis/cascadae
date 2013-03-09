
qx.Class.define("cascadae.Helpers",
{
  statics : 
  {
    /**
     * This functions allows to sort pids in the table.
     *
     * @param pos {var} TODOC
     * @return {Function} TODOC
     */
    buildPidComparator : function(pos)
    {
      return function(row1, row2)
      {
        var pid1 = row1[pos];
        var pid2 = row2[pos];
        pid1 = pid1.substr(2, pid1.length - 1).split(".");
        pid2 = pid2.substr(2, pid2.length - 1).split(".");
        var order = [ 0, 2, 1 ];

        for (var i=0; i<3; i++)
        {
          var num = order[i];
          var bit1 = parseInt(pid1[num]);
          var bit2 = parseInt(pid2[num]);
          if (bit1 < bit2) return -1;
          if (bit1 > bit2) return 1;
        }

        return 0;
      };
    },


    /**
     * This functions allows to sort IP adresses in the table.
     *
     * @param pos {var} TODOC
     * @return {Function} TODOC
     */
    buildIPComparator : function(pos)
    {
      return function(row1, row2)
      {
        var ip1 = row1[pos].split(".");
        var ip2 = row2[pos].split(".");

        for (var i=0; i<4; i++)
        {
          var bit1 = parseInt(ip1[i]);
          var bit2 = parseInt(ip2[i]);
          if (bit1 < bit2) return -1;
          if (bit1 > bit2) return 1;
        }

        return 0;
      };
    }
  }
});
