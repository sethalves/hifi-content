
"use strict";

/* global Entities, Script, Tablet, Vec3, MyAvatar, Model, Assets */

(function() { // BEGIN LOCAL_SCOPE

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: "http://headache.hungry.com/~seth/hifi/voxel-to-mesh-test/voxel-to-mesh-test.svg",
        text: "VtoM Test",
        sortOrder: 15
    });

    function retryingVoxelsToMesh(entityID, callback) {
        // if the polyvox mesh isn't yet computed, Entities.voxelsToMesh will start the process and call-back
        // with success set to false.
        Entities.voxelsToMesh(entityID, function(mesh, success) {
            if (success) {
                callback(mesh, true);
            } else {
                Script.setTimeout(function() {
                    retryingVoxelsToMesh(entityID, callback);
                }, 50);
            }
        });
    }

    function onClicked() {
        var nth = Math.floor((Math.random() * 1000) + 1);

        var position = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.5, z: -3.5}));
        var toTheSide = Vec3.multiplyQbyV(MyAvatar.orientation, {x: 1.2, y: 0, z: 0});
        var fileName = "/model-scripting-test-" + nth + ".obj";

        var voxelID0 = Entities.addEntity({
            type: "PolyVox",
            dimensions: { x: 2, y: 2, z: 2 },
            voxelVolumeSize: { x: 32, y: 32, z: 32 },
            position: Vec3.sum(position, toTheSide),
            rotation: MyAvatar.orientation,
            name: "polyvox mesh extract test 0",
            lifetime: 600,
            dynamic: false,
            collisionless: true,
            // voxelSurfaceStyle: 2 // cubic
            voxelSurfaceStyle: 3 // edged marching-cubes
        });

        var voxelID1 = Entities.addEntity({
            type: "PolyVox",
            dimensions: { x: 2, y: 2, z: 2 },
            voxelVolumeSize: { x: 32, y: 32, z: 32 },
            position: Vec3.subtract(position, toTheSide),
            rotation: MyAvatar.orientation,
            name: "polyvox mesh extract test 1",
            lifetime: 600,
            dynamic: false,
            collisionless: true,
            // voxelSurfaceStyle: 2 // edged cubic
            voxelSurfaceStyle: 3 // edged marching-cubes
        });


        Entities.setVoxelSphere(voxelID0, position, 1, 255);
        Entities.setVoxelSphere(voxelID0, position, 0.5, 0);
        Entities.setVoxelSphere(voxelID1, position, 1, 255);

        retryingVoxelsToMesh(voxelID0, function(mesh0) {
            retryingVoxelsToMesh(voxelID1, function(mesh1) {
                var mesh0T = Model.transformMesh(Entities.getEntityTransform(voxelID0), mesh0);
                var mesh1T = Model.transformMesh(Entities.getEntityTransform(voxelID1), mesh1);

                // this way causes the resulting obj to have two sub-parts
                // var objData = Model.meshToOBJ([mesh0T, mesh1T]);

                // this way it's just one mesh-part
                var combinedMeshes = Model.appendMeshes([mesh0T, mesh1T]);
                var objData = Model.meshToOBJ([combinedMeshes]);


                Assets.uploadData(objData, function(url, hash) {
                    Assets.setMapping(fileName, hash, function() {
                        Entities.addEntity({
                            type: "Model",
                            modelURL: "atp:" + fileName,
                            // dimensions: { x: 2, y: 2, z: 2 },
                            position: Vec3.sum(position, { x: 0, y: 2, z: 0 }),
                            // rotation: MyAvatar.orientation,
                            name: "polyvox mesh extract test model",
                            lifetime: 600,
                            dynamic: false,
                            collisionless: true
                        });
                    });
                });
            });
        });
    }

    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
