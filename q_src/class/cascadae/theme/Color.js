/* ************************************************************************

   qooxdoo - the new era of web development

   http://qooxdoo.org

   Copyright:
     2004-2008 1&1 Internet AG, Germany, http://www.1und1.de

   License:
     LGPL: http://www.gnu.org/licenses/lgpl.html
     EPL: http://www.eclipse.org/org/documents/epl-v10.php
     See the LICENSE file in the project's top-level directory for details.

   Authors:
     * Sebastian Werner (wpbasti)
     * Andreas Ecker (ecker)
     * Alexander Steitz (aback)
     * Martin Wittemann (martinwittemann)

************************************************************************ */

/**
 * Modern color theme
 */
qx.Theme.define("cascadae.theme.Color",
{
  extend : qx.theme.indigo.Color,
  colors :
  {
    // equal to "background-pane" and "background-odd"
    "table-row-background-even" : "transparent",
    "table-row-background-odd" : "transparent",
    "progressive-table-row-background-even" : "transparent",
    "progressive-table-row-background-odd"  : "transparent"
  }
});
