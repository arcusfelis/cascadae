/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

qx.Theme.define("cascadae.theme.Decoration",
{
  extend      : qx.theme.indigo.Decoration,
  decorations : {
    "focused-statusbar" :
    {
      decorator : qx.ui.decoration.Single,

      style :
      {
        widthTop : 1,
        colorTop : "background-selected",
        styleTop : "solid"
      }
    }
  }
});
