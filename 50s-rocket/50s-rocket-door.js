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
            // Messages.subscribe(_this.channelKey);
        },

        clickDownOnEntity: function(entityID, mouseEvent) {
            var data = JSON.stringify({
                action: 'door',
                user: MyAvatar.sessionUUID
            });
            // Messages.sendMessage(_this.channelKey, data);
            Entities.callEntityMethod(_this.channelKey, "handleMessage", [data]);
        }
    }

    return new RocketDoor();
});
