"use strict";

/* jslint bitwise: true */
/* global Script, Entities, MyAvatar, Vec3, Quat, Mat4 */

(function() { // BEGIN LOCAL_SCOPE

    // var lifetime = -1;
    var lifetime = 600;
    var tableSections = 32;
    var tableRadius = 9;

    function calculateSectionLocations(index) {
        var sectionRelativeRotation = Quat.fromPitchYawRollDegrees(0, -360 * index / tableSections, 0);
        var sectionRotation = Quat.multiply(Mat4.extractRotation(testBaseTransform), sectionRelativeRotation);
        var sectionRelativeCenterA = Vec3.multiplyQbyV(sectionRotation, { x: -0.2, y: 1.25, z: tableRadius - 0.8 });
        var sectionRelativeCenterB = Vec3.multiplyQbyV(sectionRotation, { x: 0.2, y: 1.25, z: tableRadius - 0.8 });
        var sectionRelativeCenterSign = Vec3.multiplyQbyV(sectionRotation, { x: 0, y: 1.5, z: tableRadius + 1.0 });
        var sectionCenterA = Mat4.transformPoint(testBaseTransform, sectionRelativeCenterA);
        var sectionCenterB = Mat4.transformPoint(testBaseTransform, sectionRelativeCenterB);
        var sectionCenterSign = Mat4.transformPoint(testBaseTransform, sectionRelativeCenterSign);

        return {
            rotation: sectionRotation,
            centerA: sectionCenterA,
            centerB: sectionCenterB,
            centerSign: sectionCenterSign
        };
    }


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

        var yFlip = Quat.fromPitchYawRollDegrees(0, 180, 0);

        for (var i = 0; i < 16; i++) {
            var sectionLocations = calculateSectionLocations(i);
            var sectionRotation = sectionLocations.rotation;
            var sectionCenterA = sectionLocations.centerA;
            var sectionCenterB = sectionLocations.centerB;
            var sectionCenterSign = sectionLocations.centerSign;

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

            var signText =
                "dynamic: " + dynamic + "\n" +
                "collisionless: " + collisionless + "\n" +
                "kinematic: " + kinematic + "\n" +
                "ignoreIK: " + ignoreIK + "\n";
            var propsLabel = {
                name: "controller-tests sign " + i,
                type: "Text",
                lineHeight: 0.125,
                position: sectionCenterSign,
                rotation: Quat.multiply(sectionRotation, yFlip),
                text: signText,
                dimensions: { x: 1, y: 1, z: 0.01 },
                lifetime: lifetime,
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: false,
                    },
                    controllerTestEntity: true
                })
            };
            Entities.addEntity(propsLabel);
        }

        // add a couple equippables
        var sectionLocations16 = calculateSectionLocations(16);
        // var sectionCenterSign = sectionLocations.centerSign;
        Entities.addEntity({
            name: "controller-tests model object " + i,
            type: "Model",
            modelURL: Script.resolvePath('raygun.obj.gz'),
            position: sectionLocations16.centerA,
            rotation: sectionLocations16.rotation,
            lifetime: lifetime,
            shapeType: "box",
            dynamic: false,
            dimensions: { "x": 0.118687704205513, "y": 0.18790049850940704, "z": 0.26457399129867554 },
            gravity: { x: 0, y: -0.5, z: 0 },
            script: "http://headache.hungry.com/~seth/hifi/raygun/raygun.js",
            userData: JSON.stringify({
                grabbableKey:{
                    grabbable: true,
                    cloneLimit:10,
                    cloneable:true,
                    cloneDynamic:true,
                    cloneLifetime:lifetime
                },
                wearable:{
                    joints:{
                        LeftHand:[{x:-0.0659,y:0.14683279395103455,z:0.030722394585609436},
                                  {x:-0.5422520041465759,y:0.47700217366218567,z:0.5084763169288635,w:0.46891868114471436}],
                        RightHand:[{x:0.06325198709964752,y:0.2050173580646515,z:0.03773807734251022},
                                   {x:0.41833728551864624,y:0.4624304473400116,z:0.5238471031188965,w:-0.5799986720085144}]
                    }
                }
            })
        };


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
