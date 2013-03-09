
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
      "ip"             : this.tr("IP"),
      "port"           : this.tr("Port"),
      "state"          : this.tr("State"),
      "choke_state"    : this.tr("Choking"),
      "interest_state" : this.tr("Intersted"),
      "local_choke"    : this.tr("Is choking?")
    };

    this.base(arguments, n2c);

    var tcm = this.getTableColumnModel();
    var n2p = this.getColumnNameToPositionIndex();

    var rb = tcm.getBehavior();

    rb.set(n2p.id,               { width:"1*", minWidth: 70 });
    rb.set(n2p.ip,               { width:"1*", minWidth: 100 });
    rb.set(n2p.port,             { width:"1*", minWidth: 50 });
    rb.set(n2p.torrent_id,       { width:"1*", minWidth: 30 });
    rb.set(n2p.state,            { width:"1*", minWidth: 60 });
    rb.set(n2p.interest_state,   { width:"1*", minWidth: 60 });
    rb.set(n2p.choke_state,      { width:"1*", minWidth: 60 });
    rb.set(n2p.local_choke,      { width:"1*", minWidth: 40 });

    tcm.setDataCellRenderer(n2p.local_choke, 
      new qx.ui.table.cellrenderer.Boolean());

    [ n2p.interest_state
    , n2p.choke_state
    , n2p.local_choke ].map(function(id) {
      tcm.setColumnVisible(id, false);
    });

    var tm = this.getTableModel();

    /* Set the special order of sorting in the table for composite types 
       of data. */

    tm.setSortMethods(n2p.id, cascadae.Helpers.buildPidComparator(n2p.id));
    tm.setSortMethods(n2p.ip, cascadae.Helpers.buildIPComparator(n2p.ip));
  },

  members : {}
});
