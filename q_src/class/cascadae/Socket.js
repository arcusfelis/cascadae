qx.Class.define("cascadae.Socket", {
  extend : qx.core.Object,
  events :
  {
    "connect"           : "qx.event.type.Event",
    "connect_failed"    : "qx.event.type.Event",
    "disconnect"        : "qx.event.type.Event",
    "reconnect_failed"  : "qx.event.type.Event",

    "connecting"        : "qx.event.type.Data",
    "message"           : "qx.event.type.Data",
    "close"             : "qx.event.type.Data",
    "reconnect"         : "qx.event.type.Data",
    "reconnecting"      : "qx.event.type.Data",
    "error"             : "qx.event.type.Data",

    // cascadae events
    "rd_logEvent"       : "qx.event.type.Data"
  },

  properties:
  {
    /**
     * The url used to connect to socket.io
     */
    url:
    {
      nullable:   false,
      // Use a protocol relative URL.
      init:       "//" + window.location.hostname,
      check:      "String"
    },
    /** The port used to connect */
    port:
    {
      nullable:   false,
      init:       parseInt(window.location.port),
      check:      "Number"
    },
    /** The namespace (socket.io namespace), can be empty */
    namespace:
    {
      nullable:   true,
      init:       "",
      check:      "String"
    },
    /** The socket (socket.io), can be null */
    socket:
    {
      nullable:   true,
      init:       null,
      check:      "Object"
    },
    /** Parameter for socket.io indicating if we should reconnect or not */
    reconnect:
    {
      nullable:   true,
      init:       true,
      check:      "Boolean"
    },
    connectTimeout:
    {
      nullable:   true,
      init:       10000,
      check:      "Number"
    },
    /** Reconnection delay for socket.io. */
    reconnectionDelay:
    {
      nullable:   false,
      init:       500,
      check:      "Number"
    },
    /** Max reconnection attemps */
    maxReconnectionAttemps:
    {
      nullable:   false,
      init:       1000,
      check:      "Number"
    }
  },

  /** Constructor
   *
   * @param namespace {string ? null} The namespace to connect on
   */
  construct: function(namespace)
  {
    this.base(arguments);
    if(namespace !== null)
    {
      this.setNamespace(namespace);
    }
    this.__name = [];
  },

  members:
  {
    /**
     * Trying to using socket.io to connect and plug every event from socket.io to qooxdoo one
     */
   connect: function()
   {
      if(this.getSocket() != null)
      {
        this.getSocket().removeAllListeners();
        this.getSocket().disconnect();
      }
      var address = this.getUrl() + this.getNamespace();
      this.info("Connect to " + address);
      var socket = io.connect(address, {
        'port': this.getPort(),
        'reconnect': this.getReconnect(),
        'connect timeout' : this.getConnectTimeout(),
        'reconnection delay': this.getReconnectionDelay(),
        'max reconnection attempts': this.getMaxReconnectionAttemps(),
        'force new connection': true
      })
      this.setSocket(socket);
      var self = this;

      ["connect", "connect_failed", "disconnect", "reconnect_failed"]
      .map(function(eventName) {
        socket.on(eventName, function() {
          self.info("Event ", eventName);
          self.fireEvent(eventName);
        });
      });

      ["connecting", "message", "close", "reconnect", "reconnecting", "error",

       "rd_logEvent"]
      .map(function(eventName) {
        socket.on(eventName, function(e) {
          self.info("Event ", eventName, " ", e);
          self.fireDataEvent(eventName, e);
        });
      });

      socket.on("addListener", function(e) {
        self.__addListenerFromErlang(e.name, e.hash);
     });

      socket.on("fireEvent", function(e) {
        self.__fireEventFromErlang(e.name, e.hash);
     });

      socket.on("fireDataEvent", function(e) {
        self.__fireDataEventFromErlang(e.name, e.hash, e.data);
     });
    },

    __addListenerFromErlang: function(eventName, objHash)
    {
      var item = qx.core.ObjectRegistry.fromHashCode(objHash);
      item.addListener(eventName, this.__listener, this);
    },

    __fireEventFromErlang: function(eventName, objHash)
    {
      var item = qx.core.ObjectRegistry.fromHashCode(objHash);
      item.fireEvent(eventName);
    },

    __fireDataEventFromErlang: function(eventName, objHash, eventData)
    {
      this.info("fireDataEvent ", eventName);
      console.dir(eventData);
      var item = qx.core.ObjectRegistry.fromHashCode(objHash);
      item.fireDataEvent(eventName, eventData);
    },


    /* Receive events from qooxdoo objects */
    __listener : function(e)
    {
      var item = e.getTarget();
      var meta = {};
      meta.hash = item.toHashCode();
      switch (e.classname)
      {
        case "qx.event.type.Data":
          meta.data = e.getData();
          break;
      }
      this.emit(e.getType(), meta);
    },


    /**
     * Emit an event using socket.io
     *
     * @param name {string} The event name to send to Node.JS
     * @param jsonObject {object} The JSON object to send to socket.io as parameters
    */
    emit: function(name, jsonObject)
    {
      this.getSocket().emit(name, jsonObject);
    },

    sendJSON: function(jsonObject) {
      this.getSocket().json.send(jsonObject);
    },

    /* Initially send information about the object */
    registerObject : function(item)
    {
      var data = {
        "hash"      : item.toHashCode(),
        "path"      : item.classname.split(".")
      };
      this.emit("registerObject", data);
    },

    reconnect : function()
    {
    },

    reload : function()
    {
    }
  },

  /**
   * Destructor
   */
  destruct : function()
  {
    var socket = this.getSocket();
    if(socket != null)
    {
      this.removeAllBindings();

      //Disconnecting socket.io
      socket.disconnect();
      ["connect", "connect_failed", "disconnect", "reconnect_failed",
       "connecting", "message", "close", "reconnect", "reconnecting", "error",

       "rd_logEvent"]
      .map(function(eventName) {
        socket.removeAllListeners(eventName);
      });
    }
  }
});
