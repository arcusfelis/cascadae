qx.Class.define("cascadae.ShowMagnetLinkWindow",
{
  extend : qx.ui.window.Window,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function()
  {
    this.base(arguments);

    var form = new qx.ui.form.Form();
    var links_ta = new qx.ui.form.TextArea().set({
          height: 250
    });
    form.add(links_ta, "Links", null, "links");

    this.__links_ta = links_ta;

    this.add(new qx.ui.form.renderer.Single(form));
    this.__form = form;

    var layout = new qx.ui.layout.Basic();
    this.setLayout(layout);
    this.addListenerOnce("resize", this.center, this);
  },
  members: {
    setLinks : function(links)
    {
      var urls = [];
      for (var id in links)
      {
        var url = links[id];
        urls[urls.length] = url;
      }
      this.__links_ta.setValue(urls.join("\n"));
    }
  },
  events : {
  }
});
