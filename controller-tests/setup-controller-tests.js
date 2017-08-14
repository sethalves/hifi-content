"use strict";

/* global Entities, MyAvatar, Vec3, Script, Mat4 */

(function() { // BEGIN LOCAL_SCOPE

    // var lifetime = -1;
    var lifetime = 600;

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

        for (var i = 0; i < 4; i++) {

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
