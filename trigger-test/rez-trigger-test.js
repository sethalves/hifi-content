"use strict";

/* global Entities, MyAvatar, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    var avRot = MyAvatar.orientation;
    var avRotEulers = Quat.safeEulerAngles(avRot);
    var lifetime = 600;

    var width = 0.2;
    var height = 0.2;
    var thickness = 0.08;
    var lightSize = 0.02;

    var baseID = Entities.addEntity({
        name: "trigger test base",
        type: "Box",
        color: { red: 128, green: 128, blue: 128 },
        dimensions: { x: width, y: height, z: thickness },
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.1, z: -1.2 })),
        rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
                                Quat.fromVec3Degrees({ x: -90, y: 0, z: 0 })),
        dynamic: 0,
        userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: true } }),
        lifetime: lifetime
    });


    var nearTriggerLights = [
        Entities.addEntity({
            name: "trigger test nearTrigger start",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: -width/4, y: -height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test nearTrigger continue",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: 0, y: -height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test nearTrigger end",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: width/4, y: -height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        })
    ];

    var farTriggerLights = [
        Entities.addEntity({
            name: "trigger test farTrigger start",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: -width/4, y: 0, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test farTrigger continue",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: 0, y: 0, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test farTrigger end",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: width/4, y: 0, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        })
    ];

    var mouseDownLights = [
        Entities.addEntity({
            name: "trigger test mouseDown start",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: -width/4, y: height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test mouseDown continue",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: 0, y: height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        }),
        Entities.addEntity({
            name: "trigger test mouseDown end",
            type: "Sphere",
            color: { red: 128, green: 0, blue: 0 },
            dimensions: { x: lightSize, y: lightSize, z: lightSize },
            localPosition: { x: width/4, y: height/4, z: thickness/2 },
            parentID: baseID,
            dynamic: 0,
            userData: JSON.stringify({ grabbableKey: { grabbable: false, wantsTrigger: false } }),
            lifetime: lifetime
        })
    ];

    Entities.editEntity(baseID, {
        userData: JSON.stringify({
            triggerTest: {
                near: nearTriggerLights,
                far: farTriggerLights,
                mouse: mouseDownLights
            }
        })
    });

    // do this after setting userData so preload can get entityIDs
    Entities.editEntity(baseID, {
        script: Script.resolvePath("trigger-test.js")
    });

}()); // END LOCAL_SCOPE
