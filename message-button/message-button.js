
/* jshint strict: true */
/* jslint vars: true */
/* global Entities, Messages */


(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    var onColor = { blue: 0, green: 255, red: 0 };
    var offColor = { red: 0, green: 128, blue: 255 };

    this.turnOn = function () {
        var props = Entities.getEntityProperties(this.entityID, ["userData", "locked"]);
        var userData = props.userData;
        var locked = props.locked;
        if (locked) {
            Entities.editEntity(this.entityID, { locked: false });
        }
        Entities.editEntity(this.entityID, { color: onColor});
        if (locked) {
            Entities.editEntity(this.entityID, { locked: true });
        }

        try {
            var userDataParsed = JSON.parse(userData);
            if (!userDataParsed.messageButton) {
                print("message-button.js " + this.entityID + " userData missing 'messageButton' key");
            } else if (!userDataParsed.messageButton.channel) {
                print("message-button.js " + this.entityID + " userData missing 'messageButton.channel' key");
            } else if (!userDataParsed.messageButton.message) {
                print("message-button.js " + this.entityID + " userData missing 'messageButton.message' key");
            } else {
                Messages.sendMessage(userDataParsed.messageButton.channel, userDataParsed.messageButton.message);
            }
        } catch (err) {
            print("message-button.js " + this.entityID + " userData isn't JSON?" + JSON.stringify(err));
        }
    };

    this.turnOff = function () {
        var props = Entities.getEntityProperties(this.entityID, ["locked"]);
        var locked = props.locked;
        if (locked) {
            Entities.editEntity(this.entityID, { locked: false });
        }
        Entities.editEntity(this.entityID, { color: offColor});
        if (locked) {
            Entities.editEntity(this.entityID, { locked: true });
        }
    };

    this.startNearTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.stopNearTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.startFarTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.stopFarTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };
})
