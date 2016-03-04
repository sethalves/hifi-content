//
//
//

(function() {
    // Script.include("../../libraries/utils.js");
    // Script.include("../../libraries/constants.js");

    var _this;
    var DISABLE_LASER_THRESHOLD = 0.2;
    var TRIGGER_CONTROLS = [
        Controller.Standard.LT,
        Controller.Standard.RT,
    ];
    var RELOAD_THRESHOLD = 0.95;

    RocketDoorControl = function() {
        _this = this;
        this.equipped = false;
    };

    RocketDoorControl.prototype = {
        canShoot: false,

        setChannelKey: function(id, params) {
            var newChannelKey = params[0];
            this.channelKey = newChannelKey;
            // Messages.subscribe(this.channelKey);
        },

        startEquip: function(id, params) {
            this.equipped = true;
            this.hand = params[0] == "left" ? 0 : 1;
        },

        continueEquip: function(id, params) {
            if (!this.equipped) {
                return;
            }
            this.toggleWithTriggerPressure();
        },

        toggleWithTriggerPressure: function() {
            this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[this.hand]);

            if (this.triggerValue < RELOAD_THRESHOLD) {
                this.canShoot = true;
            }
            if (this.canShoot === true && this.triggerValue === 1) {
                this.fire();
                this.canShoot = false;
            }
        },

        releaseEquip: function(id, params) {
            this.hand = null;
            this.equipped = false;
        },

        triggerPress: function(hand, value) {
            if (this.hand === hand && value === 1) {
                //We are pulling trigger on the hand we have the gun in, so fire
                this.fire();
            }
        },

        fire: function() {
            var data = JSON.stringify({
                action: 'door',
                user: MyAvatar.sessionUUID
            });
            // Messages.sendMessage(this.channelKey, data);
            print("rocket door remote clicked, channel = '" + this.channelKey + "'");
            Entities.callEntityMethod(this.channelKey, "handleMessage", [data]);
        },

        unload: function() {
        },

        preload: function(entityID) {
            this.entityID = entityID;
        },
    };

    // entity scripts always need to return a newly constructed object of our type
    return new RocketDoorControl();
});
