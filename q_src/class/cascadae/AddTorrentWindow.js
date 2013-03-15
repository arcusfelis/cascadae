qx.Class.define("cascadae.AddTorrentWindow",
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
    var address_ta = new qx.ui.form.TextArea().set({
          height: 250
    });
    var paused_cb = new qx.ui.form.CheckBox();
    form.add(address_ta, "Address", null, "address");
    form.add(paused_cb, "Pause", "paused");

    var saveButton = this.__saveButton = new qx.ui.form.Button(this.tr("Save"));
    saveButton.addListener("execute", this.__save, this);
    form.addButton(saveButton);

    this.add(new qx.ui.form.renderer.Single(form));
    this.__form = form;

    var layout = new qx.ui.layout.Basic();
    this.setLayout(layout);

    var skeleton = {address: null, paused: null};
    var model = qx.data.marshal.Json.createModel(skeleton, true);

    // create the controller and connect all fields
    var controller = new qx.data.controller.Object(model);
    controller.addTarget(address_ta, "value", "address", true);
    controller.addTarget(paused_cb, "value", "paused", true);

    this.addListenerOnce("resize", this.center, this);

    this.__model = model;
    var resetter = this.__resetter = new qx.ui.form.Resetter();
    resetter.add(address_ta);
    resetter.add(paused_cb);
  },
  members: {
    __save: function()
    {
      var data = qx.util.Serializer.toNativeObject(this.__model);
      this.fireDataEvent("submitData", data);
      this.__resetter.reset();
      this.close();
    }
  },
  events : {
    "submitData" : "qx.event.type.Data"
  }
});
