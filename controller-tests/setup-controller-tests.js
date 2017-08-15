"use strict";

/* jslint bitwise: true */
/* global Script, Entities, MyAvatar, Vec3, Quat, Mat4 */

(function() { // BEGIN LOCAL_SCOPE

    // var lifetime = -1;
    var lifetime = 600;
    var tableSections = 32;
    var tableRadius = 9;

    function setupControllerTests(testBaseTransform) {
        // var tableID =
        Entities.addEntity({
            name: "controller-tests table",
            type: "Model",
            modelURL: Script.resolvePath('controller-tests-table.obj.gz'),
            position: Mat4.transformPoint(testBaseTransform, { x: 0, y: 1, z: 0 }),
            rotation: Mat4.extractRotation(testBaseTransform),
            userData: JSON.stringify({
                grabbableKey: { grabbable: false },
                soundKey: {
                    url: "http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav",
                    volume: 0.4,
                    loop: true,
                    playbackGap: 0,
                    playbackGapRange: 0
                },
                controllerTestEntity: true
            }),
            shapeType: "static-mesh",
            lifetime: lifetime
        });

        var Xdynamic = 1;
        var Xcollisionless = 2;
        var Xkinematic = 4;
        var XignoreIK = 8;

        for (var i = 0; i < 16; i++) {
            var sectionRelativeRotation = Quat.fromPitchYawRollDegrees(0, -360 * i / tableSections, 0);
            var sectionRotation = Quat.multiply(Mat4.extractRotation(testBaseTransform), sectionRelativeRotation);
            var sectionRelativeCenterA = Vec3.multiplyQbyV(sectionRotation, { x: -0.2, y: 1.25, z: tableRadius - 0.8 });
            var sectionRelativeCenterB = Vec3.multiplyQbyV(sectionRotation, { x: 0.2, y: 1.25, z: tableRadius - 0.8 });
            var sectionCenterA = Mat4.transformPoint(testBaseTransform, sectionRelativeCenterA);
            var sectionCenterB = Mat4.transformPoint(testBaseTransform, sectionRelativeCenterB);

            var dynamic = (i & Xdynamic) ? true : false;
            var collisionless = (i & Xcollisionless) ? true : false;
            var kinematic = (i & Xkinematic) ? true : false;
            var ignoreIK = (i & XignoreIK) ? true : false;

            var propsModel = {
                name: "controller-tests model object " + i,
                type: "Model",
                modelURL: Script.resolvePath('color-cube.obj'),

                position: sectionCenterA,
                rotation: sectionRotation,

                gravity: (dynamic && !collisionless) ? { x: 0, y: -1, z: 0 } : { x: 0, y: 0, z: 0 },
                dimensions: { x: 0.2, y: 0.2, z: 0.2 },
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: true,
                        kinematic: kinematic,
                        ignoreIK: ignoreIK
                    },
                    controllerTestEntity: true
                }),
                lifetime: lifetime,
                shapeType: "box",
                dynamic: dynamic,
                collisionless: collisionless
            };
            Entities.addEntity(propsModel);

            var propsCube = {
                name: "controller-tests cube object " + i,
                type: "Box",
                shape: "Cube",
                color: { "blue": 200, "green": 10, "red": 20 },
                position: sectionCenterB,
                rotation: sectionRotation,
                gravity: dynamic ? { x: 0, y: -1, z: 0 } : { x: 0, y: 0, z: 0 },
                dimensions: { x: 0.2, y: 0.2, z: 0.2 },
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: true,
                        kinematic: kinematic,
                        ignoreIK: ignoreIK
                    },
                    controllerTestEntity: true
                }),
                lifetime: lifetime,
                shapeType: "box",
                dynamic: dynamic,
                collisionless: collisionless
            };
            Entities.addEntity(propsCube);
        }
    }

    // This assumes the avatar is standing on a flat floor with plenty of space.
    // Find the floor:
    var pickRay = {
        origin: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 2, z: -1 })),
        direction: { x: 0, y: -1, z: 0 },
        length: 20
    };
    var intersection = Entities.findRayIntersection(pickRay, true, [], [], true);

    if (intersection.intersects) {
        var testBaseTransform = Mat4.createFromRotAndTrans(MyAvatar.rotation, intersection.intersection);
        setupControllerTests(testBaseTransform);
    }

    Script.scriptEnding.connect(function () {
        var nearbyEntities = Entities.findEntities(MyAvatar.position, 30);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyUserData = Entities.getEntityProperties(nearbyID, ['userData']).userData;
            try {
                var userData = JSON.parse(nearbyUserData);
                if (userData.controllerTestEntity) {
                    Entities.deleteEntity(nearbyID);
                }
            } catch (e) {
            }
        }
    });
}()); // END LOCAL_SCOPE
