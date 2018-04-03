
"use strict";

/* global Script, Entities, Quat */

(function() {

    var self = this;

    self.preload = function (entityID) {
        self.entityID = entityID;

        var userData = Entities.getEntityProperties(self.entityID, 'userData').userData;
        var data = JSON.parse(userData);
        self.hourHandID = data.hourHandID;
        self.minuteHandID = data.minuteHandID;
        self.secondHandID = data.secondHandID;
    };

    print("clock script starting...");

    Script.setInterval(function () {
        var today = new Date();
        var hours = today.getUTCHours();
        var minutes = today.getUTCMinutes();
        var seconds = today.getUTCSeconds();

        // print("updating clock hands: " + hours + ", " + minutes + ", " + seconds);
        // ", minuteHandID=" + self.minuteHandID + ", hourHandID=" + self.hourHandID);

        Entities.editEntity(self.hourHandID, {
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI + (-2 * Math.PI * (hours + minutes / 60) / 12), 0),
        });
        Entities.editEntity(self.minuteHandID, {
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI + (-2 * Math.PI * (minutes + seconds / 60) / 60), 0),
            localAngularVelocity: { x: 0, y: -2 * Math.PI / (60 * 60), z: 0 }, // this is too close to zero and fails.
            angularDamping: 0.0
        });
        Entities.editEntity(self.secondHandID, {
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI + (-2 * Math.PI * seconds / 60), 0),
            localAngularVelocity: { x: 0, y: -2 * Math.PI / 60, z: 0 },
            angularDamping: 0.0
        });
    }, 10000); // 10 second
});
