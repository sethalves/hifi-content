
"use strict";

/* global Script, Entities, Quat */

(function() {

    var self = this;

    self.resetCounter = 0;

    self.preload = function (entityID) {
        self.entityID = entityID;

        var userData = Entities.getEntityProperties(self.entityID, 'userData').userData;
        var data = JSON.parse(userData);
        self.hourHandID = data.hourHandID;
        self.minuteHandID = data.minuteHandID;
        self.secondHandID = data.secondHandID;
    };

    self.resetHands = function () {

        var useATP = false;
        var hourHandModelURL;
        var minuteHandModelURL;
        var secondHandModelURL;
        var clockID = self.entityID;

        if (useATP) {
            hourHandModelURL = "atp:/clock/hour-hand.obj.gz";
            minuteHandModelURL = "atp:/clock/minute-hand.obj.gz";
            secondHandModelURL = "atp:/clock/second-hand.obj.gz";
        } else {
            hourHandModelURL = Script.resolvePath("hour-hand.obj.gz");
            minuteHandModelURL = Script.resolvePath("minute-hand.obj.gz");
            secondHandModelURL = Script.resolvePath("second-hand.obj.gz");
        }

        Entities.getChildrenIDs(clockID).forEach(function(childID) {
            Entities.deleteEntity(childID);
        });

        self.hourHandID = Entities.addEntity({
            name: "clock hour hand",
            type: "Model",
            modelURL: hourHandModelURL,
            registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI, 0),
            parentID: clockID,
            dimensions: { x: 0.025, y: 0.015, z: 0.25 }
        });

        self.minuteHandID = Entities.addEntity({
            name: "clock minute hand",
            type: "Model",
            modelURL: minuteHandModelURL,
            registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI, 0),
            parentID: clockID,
            dimensions: { x: 0.025, y: 0.015, z: 0.4 }
        });

        self.secondHandID = Entities.addEntity({
            name: "clock second hand",
            type: "Model",
            modelURL: secondHandModelURL,
            registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
            localPosition: { x: 0, y: 0.1, z: 0 },
            localRotation: Quat.fromPitchYawRollRadians(0, Math.PI, 0),
            parentID: clockID,
            dimensions: { x: 0.022, y: 0.015, z: 0.41 }
        });

        Entities.editEntity(clockID, {
            userData: JSON.stringify({
                hourHandID: self.hourHandID,
                minuteHandID: self.minuteHandID,
                secondHandID: self.secondHandID,
                soundKey: {
                    url: "http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav",
                    volume: 0.4,
                    loop: true,
                    playbackGap: 0,
                    playbackGapRange: 0
                }
            })
        });
    };

    print("clock script starting...");

    Script.setInterval(function () {

        self.resetCounter++;
        if (self.resetCounter > 10) {
            self.resetCounter = 0;
            self.resetHands();
        }

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
