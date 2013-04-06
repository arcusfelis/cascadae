/* ************************************************************************

   Copyright:

   License:

   Authors:

#asset(cascadae/icon/22/plus.png)
#asset(cascadae/icon/22/minus.png)

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
          padding : [2, 5],
          backgroundColor : "table-pane"
        };
      }
    },

    "treevirtual-contract" :
    {
      style : function(states)
      {
        return {
          icon : "cascadae/icon/22/minus.png"
        }
      }
    },

    "treevirtual-expand" :
    {
      style : function(states)
      {
        return {
          icon : "cascadae/icon/22/plus.png"
        }
      }
    },

    "table-scroller/pane" :
    {
      style : function(states)
      {
        return {
          backgroundColor : "table-pane"
        };
      }
    }
  }
});

