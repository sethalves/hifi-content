
/*global Script, Settings, MyAvatar, Entities, Controller, Messages, Vec3, getControllerWorldLocation */


(function() {
    Script.include("/~/system/libraries/controllers.js");

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntitiesAuto = EUs.propertiesToEntitiesAuto;
    var getConnectedEntityIDs = EUs.getConnectedEntityIDs;
    var propertySetsAreSimilar = EUs.propertySetsAreSimilar;

    var _this;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab

    var IN_BAG_DISTANCE = 0.2;

    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var BAG_SETTINGS = "io.highfidelity.bag";

    var bag = function() {
        _this = this;
    };

    bag.prototype = {


        preload: function(entityID) {
            _this.entityID = entityID;
            _this.previousTriggerValue = 0;
            _this.inOtherHandID = null;
            _this.equipped = false;

            // try {
            //     _this.entitiesInBagProps = JSON.parse(Settings.getValue(BAG_SETTINGS));
            // } catch (err) {
            //     // don't spam the logs
            //     _this.entitiesInBagProps = [];
            // }
        },


        startEquip: function (id, params) {
            _this.hand = params[0] == "left" ? 0 : 1;

            var mappingName = 'Bag-Mapping-' + "-" + Math.random();
            _this.menuMapping = Controller.newMapping(mappingName);

            if (_this.hand == LEFT_HAND) {
                _this.menuMapping.from(Controller.Standard.RT).peek().to(function (value) { _this.trigger(value); });
            } else {
                _this.menuMapping.from(Controller.Standard.LT).peek().to(function (value) { _this.trigger(value); });
            }

            Controller.enableMapping(mappingName);

            _this.equipped = true;
        },


        releaseEquip: function (id, params) {
            _this.equipped = false;
            _this.menuMapping.disable();
        },


        getPropsFromSettings: function () {
            var entitiesInBagProps;
            try {
                entitiesInBagProps = JSON.parse(Settings.getValue(BAG_SETTINGS));
            } catch (err) {
                // don't spam the logs
                entitiesInBagProps = [];
            }

            return entitiesInBagProps;
        },


        savePropsToSettings: function (entitiesInBagProps) {
            Settings.setValue(BAG_SETTINGS, JSON.stringify(entitiesInBagProps));
        },


        saveEntityInBag: function (targetEntityID) {

            // store a new entity in the bag
            var entityIDs = getConnectedEntityIDs(targetEntityID);

            for (var k = 0; k < entityIDs.length; k++) {
                if (entityIDs[k] == this.entityID) {
                    // just... no.
                    print("WARNING: bagES.js -- refusing to store bag in itself");
                    return;
                }
            }

            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.RightHand : Controller.Standard.LeftHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            var props = entitiesIDsToProperties(entityIDs, controllerLocation.position, controllerLocation.rotation);
            if (!props) {
                print("WARNING: bagES.js -- got null properties for IDs: " + JSON.stringify(entityIDs));
                return;
            }

            var entitiesInBagProps = this.getPropsFromSettings();
            var dup = false;
            for (var j = 0; j < entitiesInBagProps.length; j++) {
                var alreadyInBagProps = entitiesInBagProps[j];
                if (propertySetsAreSimilar(alreadyInBagProps, props)) {
                    // don't put duplicates into the bag
                    dup = true;
                    break;
                }
            }

            if (!dup) {
                entitiesInBagProps.push(props);
                this.savePropsToSettings(entitiesInBagProps);
            }

            for (var i = 0; i < entityIDs.length; i++) {
                Entities.deleteEntity(entityIDs[i]);
            }
        },


        takeEntityFromBag: function () {
            var entitiesInBagProps = this.getPropsFromSettings();
            if (entitiesInBagProps.length < 1) {
                return;
            }

            if (this.inOtherHandID) {
                return;
            }

            var fromBagEntityProps = entitiesInBagProps.pop();

            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.RightHand : Controller.Standard.LeftHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            propertiesToEntitiesAuto(fromBagEntityProps, controllerLocation.position, controllerLocation.rotation);

            Settings.setValue(BAG_SETTINGS, JSON.stringify(entitiesInBagProps));
        },


        trigger: function (value) {
            if (this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
                this.activate();
            }
            this.previousTriggerValue = value;
        },


        checkForGesture: function () {
            var bagPosition;
            var handPosition;

            // bagPosition = Entities.getEntityProperties(this.entityID, ["position"]).position;

            if (this.hand === RIGHT_HAND) {
                handPosition = getControllerWorldLocation(Controller.Standard.LeftHand, true).position;
                bagPosition = getControllerWorldLocation(Controller.Standard.RightHand, true).position;
            } else {
                handPosition = getControllerWorldLocation(Controller.Standard.RightHand, true).position;
                bagPosition = getControllerWorldLocation(Controller.Standard.LeftHand, true).position;
            }

            return Vec3.distance(bagPosition, handPosition) < IN_BAG_DISTANCE;
        },


        noteGrab: function (grabbedEntityID, hand) {
            if (hand == this.hand) {
                return;
            }
            this.inOtherHandID = grabbedEntityID;
        },


        activate: function () {
            if (this.checkForGesture()) {
                this.takeEntityFromBag();
            }
        },


        checkRelease: function (targetEntityID) {
            this.inOtherHandID = null;
            if (this.equipped && this.checkForGesture()) {
                this.saveEntityInBag(targetEntityID);
            }
        },
    };


    var bagInstance = new bag();


    function handleMessage(channel, message, sender) {
        var data;
        if (sender === MyAvatar.sessionUUID) {
            if (channel === 'Hifi-Object-Manipulation') {
                try {
                    data = JSON.parse(message);
                    if (data.action == "release") {
                        bagInstance.checkRelease(data.grabbedEntity);
                    } else if (data.action == "grab" || data.action == "equip") {
                        bagInstance.noteGrab(data.grabbedEntity, data.joint == "RightHand" ? RIGHT_HAND : LEFT_HAND);
                    }
                } catch (err) {
                    print("WARNING: bagES.js -- error reacting to Hifi-Object-Manipulation message: " + message);
                    print(err.message);
                }
            }
        }
    }
    Messages.subscribe('Hifi-Object-Manipulation');
    Messages.messageReceived.connect(handleMessage);


    return bagInstance;
});
