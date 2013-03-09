/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(cascadae/*)

************************************************************************ */

/**
 * This is the main application class of your custom application "cascadae"
 */
qx.Class.define("cascadae.Application",
{
  extend : qx.application.Standalone,

  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    /**
     * This method contains the initial application code and gets called 
     * during startup of the application
     *
     * @lint ignoreDeprecated(alert)
     */
    main : function()
    {
      // Call super class
      this.base(arguments);
      this.getRoot().setNativeContextMenu(true);

      // Enable logging in debug variant
//    if (qx.core.Environment.get("qx.debug"))
//    {
        // support native logging capabilities, e.g. Firebug for Firefox
        qx.log.appender.Native;

        // support additional cross-browser console. 
        // Press F7 to toggle visibility
//      qx.log.appender.Console;
//    }


      /*
      -------------------------------------------------------------------------
        Below is your actual application code...
      -------------------------------------------------------------------------
      */

      var socket = new cascadae.Socket(null);
      socket.connect();

      // Initialize the compositor
      this.__container = new cascadae.Container(this, socket);
      this.getRoot().add(this.__container, { edge : 0 });
    },

    finalize: function()
    {
      this.__container.finalize();
    }
  }
});
