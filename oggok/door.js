
/* global Entities, Quat */

(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
    };

    this.turnOn = function () {
        var rotation = Entities.getEntityProperties(this.entityID, ["rotation"]).rotation;
        var rotEulers = Quat.safeEulerAngles(rotation); // degrees

        if (rotEulers.y > 5) {
            // it's open, so close it
            Entities.addAction("tractor", this.entityID, {
                targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
                angularTimeScale: 0.5,
                tag: "oggok close door",
                ttl: 2.5
            });
        } else {
            // it's closed, so open it
            Entities.addAction("tractor", this.entityID, {
                targetRotation: Quat.fromPitchYawRollDegrees(0, 90, 0),
                angularTimeScale: 0.5,
                tag: "oggok open door",
                ttl: 2.5
            });
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
});
