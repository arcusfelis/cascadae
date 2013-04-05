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

qx.Class.define("cascadae.speedControl.ExpSlider",
{
  extend : qx.core.Object,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function()
  {
    this.base(arguments);
  },


  /*
  *****************************************************************************
     EVENTS
  *****************************************************************************
  */


  events :
  {
    /**
     * Change event for the value.
     */
    changeValue: 'qx.event.type.Data'
  },



  /*
  *****************************************************************************
     PROPERTIES
  *****************************************************************************
  */

  properties :
  {
    /**
     * The current slider value.
     *
     * Strictly validates according to {@link #minimum} and {@link #maximum}.
     * Do not apply any value correction to the incoming value. If you depend
     * on this, please use {@link #slideTo} instead.
     */
    value :
    {
      check : "typeof value==='number'&&value>=this.getMinimum()&&value<=this.getMaximum()",
      init : 0,
      apply : "_applyValue",
      nullable: true,
      event: "changeValue"
    },


    /**
     * The minimum slider value (may be negative). This value must be smaller
     * than {@link #maximum}.
     */
    minimum :
    {
      check : "Integer",
      init : 0,
      apply : "_applyMinimum"
    },


    /**
     * The maximum slider value (may be negative). This value must be larger
     * than {@link #minimum}.
     */
    maximum :
    {
      check : "Integer",
      init : 100,
      apply : "_applyMaximum"
    },

    singleStep :
    {
      check : "Integer",
      init : 10
    },

    slider :
    {
      check : "Object",
      init : null,
      apply : "_applySlider"
    },

    syncDisabled :
    {
      check : "Boolean",
      init : false
    }
  },



  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    __value2position : function(value) {
      // value = (Math.exp(Math.E * norm_pos) - 1) / 15
      // value * 15 + 1 = Math.exp(Math.E * norm_pos)
      // Math.log(value * 15 + 1) / Math.E = norm_pos
      var norm_value = this.__normalizePosition(value, this); // 0..1
      var exp_norm_value = Math.log(norm_value * 15 + 1) / Math.E;
//    console.log("value=" + value + ", norm_value=" + norm_value + ", exp_norm_value=" + exp_norm_value);
      return this.__concreteValue(exp_norm_value, this.getSlider());
    },

    __position2value : function(pos) {
      var slider = this.getSlider();
      var norm_pos_exp = this.__normalizePosition(pos, slider);
      var norm_pos = (Math.exp(Math.E * norm_pos_exp) - 1) / 15; // 0 .. 1
      var value = this.__concreteValue(norm_pos, this);
      return this.__normalizeValue(value);
    },

    /**
     * Convert a position of the slider to a value from 0 to 1.
     */
    __normalizePosition: function(pos, obj) {
      var max = obj.getMaximum(),
          min = obj.getMinimum(),
          range = max - min;
      return (pos - min) / range; // 0 .. 1
    },


    /**
     * Convert norm_pos=[0..1] to value=[min..max].
     */
    __concreteValue: function(norm_pos, obj) {
      var max = obj.getMaximum(), min = obj.getMinimum(), range = max - min;
      return norm_pos * range + min;
    },

    __normalizeValue: function(value) {
      var max = this.getMaximum(), min = this.getMinimum();
      // Bring into allowed range or fix to single step grid
      if (value < min) {
        value = min;
      } else if (value > max) {
        value = max;
      } else {
        var step = this.getSingleStep();
        value = min + Math.round((value - min) / step) * step;
      }
      return value;
    },

    _applySlider : function(value, old)
    {
      this._sync();
      value.addListener("changeValue", this.__onSliderValueChange, this);
    },

    __onSliderValueChange: function(e)
    {
      var is_disabled = this.getSyncDisabled();
      this.setSyncDisabled(true);
      this.setValue(this.__position2value(e.getData()));
      this.setSyncDisabled(is_disabled);
    },

    _sync : function()
    {
      if (this.getSyncDisabled() || !this.getSlider()) return;
      this.getSlider().setValue(this.__value2position(this.getValue()));
    },

    _applyValue : function(value, old)
    {
      this._sync();
    },
    
    // property apply
    _applyMinimum : function(value, old)
    {
      if (this.getValue() < value) {
        this.setValue(value);
      } else {
        this._sync();
      }
    },

    // property apply
    _applyMaximum : function(value, old)
    {
      if (this.getValue() > value) {
        this.setValue(value);
      } else {
        this._sync();
      }
    }
  }
});
