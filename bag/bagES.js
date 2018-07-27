
/*global Script, Entities, Controller, Messages, Vec3, getControllerWorldLocation */


(function() {
    Script.include("/~/system/libraries/controllers.js");

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var cleanProperties = EUs.cleanProperties;
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntities = EUs.propertiesToEntities;
    var getConnectedEntityIDs = EUs.getConnectedEntityIDs;

    var _this;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab

    var IN_BAG_DISTANCE = 0.1;

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
        },


        releaseEquip: function (id, params) {
            // var props = Entities.getEntityProperties(_this.entityID, ["localPosition", "localRotation"]);
            // print("QQQQ --- bag ---");
            // print(JSON.stringify(props.localPosition));
            // print(JSON.stringify(props.localRotation));

            _this.menuMapping.disable();
        },


        trigger: function (value) {
            if (this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
                this.activate();
            }
            this.previousTriggerValue = value;
        },


        activate: function () {
            print("QQQQ check activate");

            var bagPosition = Entities.getEntityProperties(this.entityID, ["position"]);

            var controllerName = (this.hand === RIGHT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            if (Vec3.distance(bagPosition, controllerLocation) < IN_BAG_DISTANCE) {
                print("QQQQ activate");
            }
        }
    };


    function handleMessage(channel, message, sender) {
    }
    Messages.subscribe('Hifi-Object-Manipulation');
    Messages.messageReceived.connect(handleMessage);


    return new bag();
});
