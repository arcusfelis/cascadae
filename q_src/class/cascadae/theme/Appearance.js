/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

qx.Theme.define("cascadae.theme.Appearance",
{
  extend      : qx.theme.indigo.Appearance,
  appearances : {
    "file-tree" :
    {
      alias : "treevirtual",
      include : "treevirtual",

      style : function(states, style)
      {
        return {
          // hide the border
          padding: 0,
          decorator: undefined
        };
      }
    },
    "table" :
    {
      style : function(states, style)
      {
        return {};
      }
    },

    "table/statusbar" :
    {
      style : function(states)
      {
        return {
          decorator : states.focused ? "focused-statusbar" : "statusbar",
          padding : [2, 5]
        };
      }
    }
  }
});

