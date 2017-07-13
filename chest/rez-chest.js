"use strict";

/* global Entities, MyAvatar, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    // dimensions of the chest box
    var width = 1;
    var height = 0.5;
    var depth = 0.5;

    var avRot = MyAvatar.orientation;
    var avRotEulers = Quat.safeEulerAngles(avRot);

    var chestBoxID = Entities.addEntity({
        name: "chest box",
        type: "Model",
        modelURL: "atp:/chest/chest-box.obj.gz",
        compoundShapeURL: "atp:/chest/chest-box-hull.obj.gz",
        dimensions: { x: width, y: height, z: depth },
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.2, z: -4 })),
        rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
                                Quat.fromVec3Degrees({ x: 90, y: 0, z: 0 })),
        collisionsWillMove: 1,
        dynamic: 1,
        gravity: { "x": 0, "y": -1, "z": 0 },
        shapeType: "compound",
        userData: JSON.stringify({ grabbableKey: { grabbable: true, kinematic: false }})
    });

    var chestLidID = Entities.addEntity({
        name: "chest lid",
        type: "Model",
        modelURL: "atp:/chest/chest-lid.obj.gz",
        compoundShapeURL: "atp:/chest/chest-lid-hull.obj.gz",
        dimensions: { x: width, y: 0.2378, z: depth },
        position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 1.2, z: -4 })),
        rotation: Quat.multiply(Quat.fromVec3Degrees({ x: 0, y: avRotEulers.y, z: 0 }),
                                Quat.fromVec3Degrees({ x: 90, y: 0, z: 0 })),
        collisionsWillMove: 1,
        dynamic: 1,
        gravity: { "x": 0, "y": -1, "z": 0 },
        shapeType: "compound",
        userData: JSON.stringify({ grabbableKey: { grabbable: true, kinematic: false }})
    });

    /* var hingeID = */ Entities.addAction("hinge", chestBoxID, {
        pivot: { x: 0, y: height/2 + 0.002, z: -depth/2 },
        axis: { x: 1, y: 0, z: 0 },
        otherEntityID: chestLidID,
        otherPivot: { x: 0, y: -depth / 4, z: -depth/2 },
        otherAxis: { x: 1, y: 0, z: 0 },
        tag: "chest hinge"
    });


}()); // END LOCAL_SCOPE
