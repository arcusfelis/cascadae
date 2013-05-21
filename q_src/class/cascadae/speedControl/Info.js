/* ************************************************************************

   qooxdoo - the new era of web development

   http://qooxdoo.org

   Copyright:
     2006 STZ-IDA, Germany, http://www.stz-ida.de

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php
     See the LICENSE file in the project's top-level directory for details.

   Authors:
     * Til Schneider (til132)
     * Fabian Jakobs (fjakobs)

************************************************************************ */

qx.Class.define("cascadae.speedControl.Info",
{
  extend : qx.ui.basic.Label,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function()
  {
    this.base(arguments, "Speed...");
    this.addListener("rd_dataUpdated", this.__onDataUpdated, this);
  },


  /*
  *****************************************************************************
     EVENTS
  *****************************************************************************
  */


  events :
  {
    "rd_dataUpdated"       : "qx.event.type.Data",
    "d_changeRates"        : "qx.event.type.Data"
  },


  /*
  *****************************************************************************
     PROPERTIES
  *****************************************************************************
  */

  properties :
  {
    recvRate:
    {
      nullable:   false,
      init:       0,
      check:      "Integer",
      apply:      "_applyRecvRate"
    },
    sendRate:
    {
      nullable:   false,
      init:       0,
      check:      "Integer",
      apply:      "_applySendRate"
    },
    maxRecvRate:
    {
      nullable:   false,
      init:       0,
      check:      "Integer",
      apply:      "_applyMaxRecvRate"
    },
    maxSendRate:
    {
      nullable:   false,
      init:       0,
      check:      "Integer",
      apply:      "_applyMaxSendRate"
    },
    bulkUpdate:
    {
      nullable:   false,
      init:       false,
      check:      "Boolean",
      apply:      "_applyBulkUpdate"
    }
  },


  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    __onDataUpdated: function(e)
    {
      var data = e.getData();
      this.setBulkUpdate(true);
      if (!isNaN(data.recv_rate)) this.setRecvRate(data.recv_rate);
      if (!isNaN(data.send_rate)) this.setSendRate(data.send_rate);
      if (!isNaN(data.max_recv_rate)) this.setMaxRecvRate(data.max_recv_rate);
      if (!isNaN(data.max_send_rate)) this.setMaxSendRate(data.max_send_rate);
      this.setBulkUpdate(false);
    },
    _applyRecvRate: function(value)
    {
      this.updateLabel();
    },
    _applySendRate: function(value)
    {
      this.updateLabel();
    },
    _applyBulkUpdate: function(value)
    {
      this.updateLabel();
    },
    _applyMaxRecvRate: function(value)
    {
    },
    _applyMaxSendRate: function(value)
    {
    },

    updateLabel: function()
    {
      if (this.isBulkUpdate()) return;
      var recv = this.getRecvRate();
      var send = this.getSendRate();
      this.setValue("" + this.__stringPadding(cascadae.Helpers.bytesToSize(recv, 2) + "\u2193") +
                   " " + this.__stringPadding(cascadae.Helpers.bytesToSize(send, 2) + "\u2191"));
    },

    __stringPadding: function(s)
    {
      return String("\u00a0 \u00a0 \u00a0 \u00a0 \u00a0 \u00a0 \u00a0 \u00a0 \u00a0" + s).slice(-18);
    },

    changeRates: function(new_max_bytes_rate_in, new_max_bytes_rate_out)
    {
      this.fireDataEvent("d_changeRates", {rate_in: new_max_bytes_rate_in,
                                           rate_out: new_max_bytes_rate_out});
    }
  },


  /*
  *****************************************************************************
     DESTRUCTOR
  *****************************************************************************
  */

  destruct : function() {
  }
});
