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

/**
 * The table pane that shows a certain section from a table. This class handles
 * the display of the data part of a table and is therefore the base for virtual
 * scrolling.
 */
qx.Class.define("cascadae.speedControl.Pane",
{
  extend : qx.ui.popup.Popup,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function(info)
  {
    var grid = new qx.ui.layout.Grid().set({spacing: 5});
    this.base(arguments, grid);
    this.set({
        padding: [2, 4],
        offset : 3,
        position : "top-right",
        width : 500,
        height : 200
    });

    var spinner_in = new qx.ui.form.Spinner().set({
            minimum: 10,
            maximum: 10000});
    var spinner_out = new qx.ui.form.Spinner().set({
            minimum: 10,
            maximum: 10000});
    var title_in  = new qx.ui.basic.Label("Maximum incoming speed rate (KiB/s):");
    var title_out = new qx.ui.basic.Label("Maximum outgoing speed rate (KiB/s):");
    var slider_in = new qx.ui.form.Slider().set({
            orientation: "horizontal",
            maximum: 500
    });
    var slider_out = new qx.ui.form.Slider().set({
            orientation: "horizontal",
            maximum: 500
    });
    var slider_ctl_in = new cascadae.speedControl.ExpSlider().set({
            slider: slider_in,
            minimum: 10,
            maximum: 10000,
            singleStep: 10
    });
    var slider_ctl_out = new cascadae.speedControl.ExpSlider().set({
            slider: slider_out,
            minimum: 10,
            maximum: 10000,
            singleStep: 10
    });
    slider_ctl_in.bind("value", spinner_in, "value");
    slider_ctl_out.bind("value", spinner_out, "value");
    spinner_in.bind("value", slider_ctl_in, "value");
    spinner_out.bind("value", slider_ctl_out, "value");
          
    grid.setColumnWidth(0, 80);
    grid.setColumnFlex(1, 1);
    var container = this;
    container.add(title_in, {row: 0, column: 0, colSpan: 2});
    container.add(spinner_in, {row: 1, column: 0});
    container.add(slider_in, {row: 1, column: 1});

    container.add(title_out, {row: 2, column: 0, colSpan: 2});
    container.add(spinner_out, {row: 3, column: 0});
    container.add(slider_out, {row: 3, column: 1});

    this.__info = info;
    this.__spinner_in = spinner_in;
    this.__spinner_out = spinner_out;
    this.addListener("changeVisibility", this.__onVisibilityChange, this);
  },


  /*
  *****************************************************************************
     EVENTS
  *****************************************************************************
  */


  events :
  {
  },


  /*
  *****************************************************************************
     PROPERTIES
  *****************************************************************************
  */

  properties :
  {
  },


  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    __onVisibilityChange: function()
    {
      if (this.isVisible())
      {
        this.__spinner_in.setValue (Math.round(this.__info.getMaxRecvRate() / 1024));
        this.__spinner_out.setValue(Math.round(this.__info.getMaxSendRate() / 1024));
      }
      else
      {
        var rate_in  = this.__spinner_in.getValue()  * 1024,
            rate_out = this.__spinner_out.getValue() * 1024;
        this.__info.changeRates(rate_in, rate_out);
      }
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
