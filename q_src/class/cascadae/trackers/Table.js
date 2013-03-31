
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("cascadae.trackers.Table",
{
  extend : cascadae.BasicTable,


  /**
   * @lint ignoreUndefined(qxc)
   */
  construct : function(store)
  {
    var n2c =
    {
      "id"             : this.tr("Tracker"),
      "torrent_id"     : this.tr("Torrent"),
      "tracker_url"    : this.tr("URL"),
      "tier_num"       : this.tr("Tier"),
      "last_attempted" : this.tr("Attempted"),
      "last_announced" : this.tr("Announced"),
      "message"        : this.tr("Message"),
      "message_level"  : this.tr("Level")
    };

    this.base(arguments, n2c);

    var tcm = this.getTableColumnModel();
    var n2p = this.getColumnNameToPositionIndex();

    var rb = tcm.getBehavior();

    rb.set(n2p.id,               { width:"1*", minWidth: 30 });
    rb.set(n2p.torrent_id,       { width:"1*", minWidth: 30 });
    rb.set(n2p.tracker_url,      { width:"1*", minWidth: 160 });
    rb.set(n2p.tier_num,         { width:"1*", minWidth: 20 });
    rb.set(n2p.last_attempted,   { width:"1*", minWidth: 80 });
    rb.set(n2p.last_announced,   { width:"1*", minWidth: 80 });
    rb.set(n2p.message,          { width:"1*", minWidth: 160 });
    rb.set(n2p.message_level,    { width:"1*", minWidth: 30 });

//  var format = new qx.util.format.DateFormat("HH:mm:ss dd.MM.yyyy");
    var format = new qx.util.format.DateFormat("HH:mm:ss");
    var ann_cr = new qx.ui.table.cellrenderer.Date();
    var att_cr = new qx.ui.table.cellrenderer.Date();
    ann_cr.setDateFormat(format);
    att_cr.setDateFormat(format);
    tcm.setDataCellRenderer(n2p.last_announced, ann_cr);
    tcm.setDataCellRenderer(n2p.last_attempted, att_cr);

    [ n2p.id
    , n2p.message_level
    , n2p.tier_num ].map(function(id) {
      tcm.setColumnVisible(id, false);
    });
  },

  members : {
    fillFields : function(row, newValues, add)
    {
      newValues = this.base(arguments, row, newValues, add);
      var n2p = this.getColumnNameToPositionIndex();
      newValues[n2p.last_attempted] = this.__toDate(newValues[n2p.last_attempted]);
      newValues[n2p.last_announced] = this.__toDate(newValues[n2p.last_announced]);
      return newValues;
    },

    __toDate : function(diff)
    {
      if (diff === undefined) return;
      var d = new Date();
      d.setSeconds(d.getSeconds() - diff);
      return d;
    }
  }
});
