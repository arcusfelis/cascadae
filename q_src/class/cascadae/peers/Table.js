
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("cascadae.peers.Table",
{
  extend : cascadae.BasicTable,


  /**
   * @lint ignoreUndefined(qxc)
   */
  construct : function(store)
  {
    var n2c =
    {
      "id"             : this.tr("Pid"),
      "torrent_id"     : this.tr("Id"),
      "version"        : this.tr("Version"),
      "ip"             : this.tr("IP"),
      "port"           : this.tr("Port"),
      "state"          : this.tr("State"),
      "l_choked"       : this.tr("Choked"),    // We are blocking this peer.
      "r_choked"       : this.tr("Choking"),   // We are blocked by this peer.
      "r_interested"   : this.tr("Intersted"), // This peer is interested in us.
      "recv_rate"      : this.tr("Rate In"),
      "send_rate"      : this.tr("Rate Out")
    };

    this.base(arguments, n2c);

    var tcm = this.getTableColumnModel();
    var n2p = this.getColumnNameToPositionIndex();

    var rb = tcm.getBehavior();

    rb.set(n2p.id,               { width:"1*", minWidth: 70 });
    rb.set(n2p.version,          { width:"1*", minWidth: 100 });
    rb.set(n2p.ip,               { width:"1*", minWidth: 100 });
    rb.set(n2p.port,             { width:"1*", minWidth: 50 });
    rb.set(n2p.torrent_id,       { width:"1*", minWidth: 30 });
    rb.set(n2p.state,            { width:"1*", minWidth: 60 });
    rb.set(n2p.r_interested,     { width:"1*", minWidth: 40 });
    rb.set(n2p.r_choked,         { width:"1*", minWidth: 40 });
    rb.set(n2p.l_choked,         { width:"1*", minWidth: 40 });
    rb.set(n2p.recv_rate,        { width:"1*", minWidth: 70 });
    rb.set(n2p.send_rate,        { width:"1*", minWidth: 70 });

    tcm.setDataCellRenderer(n2p.recv_rate, new cascadae.cellrenderer.Speed());
    tcm.setDataCellRenderer(n2p.send_rate, new cascadae.cellrenderer.Speed());

    [ n2p.r_interested
    , n2p.r_choked
    , n2p.l_choked ].map(function(id) {
        tcm.setDataCellRenderer(id, 
          new qx.ui.table.cellrenderer.Boolean());
    });

    [ n2p.r_interested
    , n2p.r_choked
    , n2p.l_choked ].map(function(id) {
      tcm.setColumnVisible(id, false);
    });

    var tm = this.getTableModel();

    /* Set the special order of sorting in the table for composite types 
       of data. */

    tm.setSortMethods(n2p.id, cascadae.Helpers.buildPidComparator(n2p.id));
    tm.setSortMethods(n2p.ip, cascadae.Helpers.buildIPComparator(n2p.ip));
  },

  members : {
    fillFields : function(row, newValues, add)
    {
      newValues = this.base(arguments, row, newValues, add);
      var n2p = this.getColumnNameToPositionIndex();

      if (add)
      {
        newValues[n2p.recv_rate] = 0;
        newValues[n2p.send_rate] = 0;
      }

      return newValues;
    }
  }
});
