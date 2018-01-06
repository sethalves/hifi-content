"use strict";

/* global MyAvatar, Entities, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE
    var avRotEulers = Quat.safeEulerAngles(MyAvatar.orientation);

    Entities.addEntity({
        name: "Portal",
        // lifetime: 120,
        type: "Sphere",
        color: { blue: 255, green: 255, red: 255 },
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.2, z: -4 })),
        // rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
        //                         Quat.fromVec3Degrees({ x: 90, y: 0, z: 0 })),
        rotation: Quat.fromVec3Degrees({ x: 0, y: 90, z: 0 })
        dimensions: { x: 0.8, y: 0.8, z: 0.8 },
        userData: JSON.stringify({
            grabbableKey: {
                grabbable: true
            },
            ProceduralEntity: {
                version: 2,
                shaderUrl: "http://headache.hungry.com/~seth/hifi/portal/portal.fs?v=1",
                channels: ["https://hifi-metaverse.s3-us-west-1.amazonaws.com/images/places/previews/7f8/8b1/91-/original/hifi-place-7f88b191-fe1d-4d26-8bd3-b1df36e56623.png?1496355317"]
            }
        })
    });
}()); // END LOCAL_SCOPE
