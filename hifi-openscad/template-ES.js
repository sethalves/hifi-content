

/* global Entities, Script, getEntityCustomData */


(function () {
    Script.include("/~/system/libraries/utils.js");

    var _this = this;
    _this.propertiesSet = false;

    _this.preload = function (entityID) {
        _this.entityID = entityID;
        saveOriginalProperties();
    };

    var saveOriginalProperties = function () {
        print("saveOriginalProperties for " + _this.entityID);
        _this.properties = Entities.getEntityProperties(_this.entityID);

        var customData = getEntityCustomData('template', this.entityID, {});
        if (customData && customData.script) {
            _this.scriptAfterGrabbed = customData.script;
        } else {
            _this.scriptAfterGrabbed = "";
        }

        if (_this.properties.position &&
            _this.properties.position.x !== 0 &&
            _this.properties.position.y !== 0 &&
            _this.properties.position.z !== 0) {
            _this.propertiesSet = true;
        }

        if (!_this.propertiesSet) {
            Script.setTimeout(saveOriginalProperties, 2000);
        }
    };

    var startNearGrab = function () {
        print("duplicating " + _this.entityID);
        Entities.addEntity(_this.properties);
        Entities.editEntity(_this.entityID, {script: _this.scriptAfterGrabbed});
    };

    this.startNearGrab = startNearGrab;
})
