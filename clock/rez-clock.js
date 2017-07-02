"use strict";

/* global Script, Entities, MyAvatar, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    var avRot = MyAvatar.orientation;
    var avRotEulers = Quat.safeEulerAngles(avRot);

    var clockID = Entities.addEntity({
        name: 'clock',
        type: 'Model',
        modelURL: Script.resolvePath('clock.obj'),
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.2, z: -4 })),
        // rotation: Quat.fromVec3Degrees({ x: 90, y: -avRotEulers.y, z: 0 }),
        rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
                                Quat.fromVec3Degrees({ x: -90, y: 0, z: 0 })),
        userData: JSON.stringify({
            "grabbableKey": { "grabbable": true },
            "soundKey": {
                "url": "http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav",
                "volume": 0.4,
                "loop": true,
                "playbackGap": 0,
                "playbackGapRange": 0
            }
        }),
        // lifetime: 300
    });

    var hourHandID = Entities.addEntity({
        name: 'clock hour hand',
        type: 'Model',
        modelURL: Script.resolvePath('hour-hand.obj'),
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: -0.1, z: 0 },
        parentID: clockID
    });

    var minuteHandID = Entities.addEntity({
        name: 'clock minute hand',
        type: 'Model',
        modelURL: Script.resolvePath('minute-hand.obj'),
        registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
        localPosition: { x: 0, y: -0.1, z: 0 },
        parentID: clockID
    });

    Entities.editEntity(clockID, {
        userData: JSON.stringify({
            minuteHandID: minuteHandID,
            hourHandID: hourHandID
        }),
        serverScripts: Script.resolvePath("clock.js?v=8")
    });

}()); // END LOCAL_SCOPE
