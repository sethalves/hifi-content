
/* global Messages, MyAvatar */

(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.turnOn = function () {
        Messages.sendMessage("Orrery Controls", JSON.stringify({
            action: 'update',
            position: MyAvatar.position
        }));
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
