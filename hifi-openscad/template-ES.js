var SCALE = 3.0;
var EARTH_SIZE = 3.959 / SCALE;
var MOON_SIZE = 1.079 / SCALE;


(function () {
    var _this = this;
    _this.propertiesSet = false;

    _this.preload = function (entityID) {
        _this.entityID = entityID;
        saveOriginalProperties();
    }

    var saveOriginalProperties = function () {
        print("saveOriginalProperties for " + _this.entityID);
        _this.properties = Entities.getEntityProperties(_this.entityID);
        if (_this.properties.position &&
            _this.properties.position.x != 0 &&
            _this.properties.position.y != 0 &&
            _this.properties.position.z != 0) {
            _this.propertiesSet = true;
        }

        if (!_this.propertiesSet) {
            Script.setTimeout(saveOriginalProperties, 2000);
        }
    }

    var startNearGrab = function () {
        print("duplicating " + _this.entityID);
        Entities.addEntity(_this.properties);
        Entities.editEntity(_this.entityID, {script: ""});
    }
    this.startNearGrab = startNearGrab;
})
