
/* global Messages, MyAvatar, Entities */

(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.turnOn = function () {
        var entityProps = Entities.getEntityProperties(this.entityID, ["userData"]);
        try {
            var parsedUserData = JSON.parse(entityProps.userData);
            Messages.sendMessage("Orrery Controls", JSON.stringify({
                action: parsedUserData.action,
                position: MyAvatar.position
            }));
        } catch (e) {
            print("button needs userData set");
        }
    };

    this.turnOff = function () {
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
});
