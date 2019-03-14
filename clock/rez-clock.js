"use strict";

/* global Script, Entities, MyAvatar, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    var avRot = MyAvatar.orientation;
    var avRotEulers = Quat.safeEulerAngles(avRot);

    var clockModelURL;
    var hourHandModelURL;
    var minuteHandModelURL;
    var secondHandModelURL;
    var clockServerScriptURL;

    var useATP = false;

    if (useATP) {
        clockModelURL = "atp:/clock/clock.obj.gz";
        hourHandModelURL = "atp:/clock/hour-hand.obj.gz";
        minuteHandModelURL = "atp:/clock/minute-hand.obj.gz";
        secondHandModelURL = "atp:/clock/second-hand.obj.gz";
        clockServerScriptURL = "atp:/clock/clock.js";
    } else {
        clockModelURL = Script.resolvePath("clock.obj.gz");
        hourHandModelURL = Script.resolvePath("hour-hand.obj.gz");
        minuteHandModelURL = Script.resolvePath("minute-hand.obj.gz");
        secondHandModelURL = Script.resolvePath("second-hand.obj.gz");
        clockServerScriptURL = Script.resolvePath("clock.js");
    }

    var clockID = Entities.addEntity({
        name: "clock",
        type: "Model",
        modelURL: clockModelURL,
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.2, z: -4 })),
        rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
                                Quat.fromVec3Degrees({ x: 90, y: 0, z: 0 })),
        grab: { grabbable: true }
    });

    var hourHandID = Entities.addEntity({
        name: "clock hour hand",
        type: "Model",
        modelURL: hourHandModelURL,
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: 0.1, z: 0 },
        localRotation: Quat.fromPitchYawRollRadians(0, Math.PI, 0),
        parentID: clockID,
        dimensions: { x: 0.025, y: 0.015, z: 0.25 }
    });

    var minuteHandID = Entities.addEntity({
        name: "clock minute hand",
        type: "Model",
        modelURL: minuteHandModelURL,
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: 0.1, z: 0 },
        localRotation: Quat.fromPitchYawRollRadians(0, Math.PI, 0),
        parentID: clockID,
        dimensions: { x: 0.025, y: 0.015, z: 0.4 }
    });

    var secondHandID = Entities.addEntity({
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
            hourHandID: hourHandID,
            minuteHandID: minuteHandID,
            secondHandID: secondHandID,
            soundKey: {
                url: "http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav",
                volume: 0.4,
                loop: true,
                playbackGap: 0,
                playbackGapRange: 0
            }
        }),
        serverScripts: clockServerScriptURL
    });

}()); // END LOCAL_SCOPE
