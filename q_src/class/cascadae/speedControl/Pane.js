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

  construct : function()
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
    var title_in = new qx.ui.basic.Label("Maximum incoming speed rate:");
    var slider_in = new qx.ui.form.Slider().set({
            orientation: "horizontal",
            maximum: 500
    });
    var slider_ctl_in = new cascadae.speedControl.ExpSlider().set({
            slider: slider_in,
            minimum: 10,
            maximum: 10000,
            singleStep: 10
    });
    slider_ctl_in.bind("value", spinner_in, "value");
    spinner_in.bind("value", slider_ctl_in, "value");
          
    grid.setColumnWidth(0, 80);
    grid.setColumnFlex(1, 1);
    var container = this;
    container.add(title_in, {row: 0, column: 0, colSpan: 2});
    container.add(spinner_in, {row: 1, column: 0});
    container.add(slider_in, {row: 1, column: 1});
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

  },


  /*
  *****************************************************************************
     DESTRUCTOR
  *****************************************************************************
  */

  destruct : function() {
  }
});
