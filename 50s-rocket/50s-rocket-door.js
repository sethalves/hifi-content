//
//
//

(function() {
    var _this;

    RocketDoor = function() {
        _this = this;
    };

    RocketDoor.prototype = {
        setChannelKey: function(id, params) {
            var newChannelKey = params[0];
            _this.channelKey = newChannelKey;
        },

        clickDownOnEntity: function(entityID, mouseEvent) {
            this.sendSignal();
        },

        startNearTrigger: function() {
            this.sendSignal();
        },

        sendSignal: function(entityID, mouseEvent) {
            var data = JSON.stringify({
                action: 'door',
                user: MyAvatar.sessionUUID
            });
            print("DOOR SIGNAL");
            Entities.callEntityMethod(_this.channelKey, "handleMessage", [data]);
        }
    }

    return new RocketDoor();
});
