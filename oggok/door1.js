
/* global Entities, Quat */

(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
    };

    this.turnOn = function () {
        var rotation = Entities.getEntityProperties(this.entityID, ["rotation"]).rotation;
        var rotEulers = Quat.safeEulerAngles(rotation); // degrees

        if (rotEulers.y < 175 && rotEulers.y > -175) {
            // it's open, so close it
            Entities.addAction("tractor", this.entityID, {
                targetRotation: Quat.fromPitchYawRollDegrees(0, 180, 0),
                angularTimeScale: 0.5,
                tag: "oggok close door",
                ttl: 2.5
            });
        } else {
            // it's closed, so open it
            Entities.addAction("tractor", this.entityID, {
                targetRotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
                angularTimeScale: 0.5,
                tag: "oggok open door",
                ttl: 2.5
            });
        }
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        if (mouseEvent.isPrimaryButton) {
            this.entityID = entityID;
            this.turnOn();
        }
    };
});
