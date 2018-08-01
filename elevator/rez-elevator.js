"use strict";

/* global Entities, MyAvatar, Vec3 */

(function() { // BEGIN LOCAL_SCOPE

    var lifetime = -1;

    function rezElevator(elevatorBasePosition) {
        var zoneHeight = 10;
        var zoneVerticalOffset = 1;
        var rise = 15;

        var zoneID = Entities.addEntity({
            damping: 0,
            dimensions: { x: 10, y: zoneHeight, z: 10 },
            localizedSimulation: 1,
            name: "elevator zone",
            position: Vec3.sum(elevatorBasePosition, { x: 0, y: zoneVerticalOffset + (zoneHeight / 2), z: 0}),
            serverScripts: "http://headache.hungry.com/~seth/hifi/elevator/elevator.js",
            shapeType: "box",
            type: "Zone",
            velocity: { x: 0, y: 0, z: 0 },
            userData: JSON.stringify({
                elevatorData: {
                    basePosition: elevatorBasePosition,
                    zoneHeight: zoneHeight,
                    zoneVerticalOffset: zoneVerticalOffset,
                    rise: rise,
                }
            }),
            lifetime: lifetime
        });

        // var floorID =
        Entities.addEntity({
            color: { blue: 150, green: 150, red: 150 },
            dimensions: { x: 10, y: 0.2, z: 10 },
            name: "elevator floor",
            parentID: zoneID,
            localPosition: { x: 0, y: (-zoneHeight / 2) - zoneVerticalOffset, z: 0},
            shape: "Cube",
            type: "Box",
            lifetime: lifetime
        });

        // var upperFloorID =
        Entities.addEntity({
            color: { blue: 150, green: 200, red: 150 },
            dimensions: { x: 10, y: 0.2, z: 10 },
            name: "elevator upper floor",
            position: Vec3.sum({ x: 10, y: rise, z: 0 }, elevatorBasePosition),
            shape: "Cube",
            type: "Box",
            lifetime: lifetime
        });
    }

    var pickRay = {
        origin: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 2, z: -7.5 })),
        direction: { x: 0, y: -1, z: 0 },
        length: 20
    };
    var intersection = Entities.findRayIntersection(pickRay, true, [], [], true);

    if (intersection.intersects) {
        rezElevator(intersection.intersection);
    }
}()); // END LOCAL_SCOPE
