
/* global module, Graphics, Entities, Vec3, Quat, AvatarList, Script */

function getTopMaterial(multiMaterial) {
    // For non-models: multiMaterial[0] will be the top material
    // For models, multiMaterial[0] is the base material, and multiMaterial[1] is the highest priority applied material
    if (multiMaterial.length > 1) {
        if (multiMaterial[1].priority > multiMaterial[0].priority) {
            return multiMaterial[1];
        }
    }

    return multiMaterial[0];
}

function applyMaterial(avatarID, opacity, lifetime, materialsDict) {
    var newEntities = {};
    var mesh = Graphics.getModel(avatarID);
    if (mesh) {
        var materials = mesh.materialLayers;
        for (var m in materials) {
            if (materials.hasOwnProperty(m) && parseInt(m.toString()) == m) {
                var multiMaterial = materials[m];
                if (multiMaterial) {
                    var topMaterial = getTopMaterial(multiMaterial);
                    var materialID = Entities.addEntity({
                        name: "puppet avatar-fade " + m,
                        type: "Material",
                        materialURL: "materialData",
                        materialData: JSON.stringify({
                            materialVersion: 1,
                            materials: materialsDict
                        }),
                        localPosition: Vec3.ZERO,
                        localRotation: Quat.IDENTITY,
                        parentID: avatarID,
                        priority: topMaterial.priority + 1,
                        parentMaterialName: m.toString(),
                        lifetime: lifetime
                    }, true);

                    newEntities["material-" + m] = materialID;
                }
            }
        }
    }

    return newEntities;
}


function fadeAvatar(avatarID, opacity, lifetime) {
    return applyMaterial(avatarID, opacity, lifetime, {
        model: "hifi_pbr",
        opacity: opacity,
        defaultFallthrough: true
    });
}


function greyEntity(entityID, opacity, lifetime) {
    return applyMaterial(entityID, opacity, lifetime, {
        albedo: [1.0, 1.0, 0],
        opacity: opacity,
        defaultFallthrough: true
    });
}


function freezeAvatar(avatarID, lifetime) {
    print("QQQQ freezing avatar " + JSON.stringify(avatarID) + " for " + lifetime + " seconds.");
    var newEntities = fadeAvatar(avatarID, 0.1, lifetime);

    var avatar = AvatarList.getAvatar(avatarID);

    if (avatar) {
        // var modelURL = avatar.getModelURL();
        var modelURL = avatar.skeletonModelURL;
        if (modelURL) {
            var jointRotations = avatar.getJointRotations();
            // var jointRotationsSet = [];
            var jointTranslations = avatar.getJointTranslations();

            // for (var i = 0; i < jointRotations.length; i++) {
            //     jointRotationsSet.push(true);
            // }

            // var av = AvatarManager.getAvatar(avatarID);
            // print("QQQQ hips " + JSON.stringify(avatar.getJointTranslation(avatar.getJointIndex("Hips"))) +
            //       " " + JSON.stringify(av.getJointPosition("Hips")) +
            //       " " + JSON.stringify(av.getAbsoluteJointTranslationInObjectFrame(avatar.getJointIndex("Hips")))
            //      );
            // print("QQQQ feet " + JSON.stringify(avatar.getJointTranslation(avatar.getJointIndex("RightFoot"))) +
            //       " " + JSON.stringify(av.getJointPosition("RightFoot")) +
            //       " " + JSON.stringify(av.getAbsoluteJointTranslationInObjectFrame(avatar.getJointIndex("RightFoot")))
            //      );
            // print("QQQQ pos " + JSON.stringify(avatar.position) + " " + JSON.stringify(av.position));
            // print("QQQQ off " + JSON.stringify(av.getSkeletonOffset()));


            var dopplegangerID = Entities.addEntity({
                type: "Model",
                name: "frozen avatar: " + avatar.displayName,
                position: avatar.position,
                // position: hipsPosition,
                // position: Vec3.sum(avatar.position, offset),
                rotation: Quat.multiply(Quat.fromPitchYawRollDegrees(0, 180, 0), avatar.orientation),
                // rotation: Quat.fromPitchYawRollRadians(0, avatar.bodyYaw, 0),
                // avatar.scale
                // dimensions: { x: 0.5, y: 0.5, z: 0.5 },
                collisionless: true,
                dynamic: 0,
                modelURL: modelURL,
                lifetime: lifetime
                // jointRotations: jointRotations,
                // jointRotationsSet: jointRotationsSet
            });
            newEntities.doppleganger = dopplegangerID;

            var tries = 0;
            var setJoints = function () {
                tries += 1;
                if (tries > (lifetime * 1000) / 80.0) {
                    return;
                }
                var rotsOK = Entities.setLocalJointRotations(dopplegangerID, jointRotations);
                var offsOK = Entities.setLocalJointTranslations(dopplegangerID, jointTranslations);
                if (rotsOK && offsOK) {

                    var modelHips =
                        Entities.getAbsoluteJointTranslationInObjectFrame(dopplegangerID, avatar.getJointIndex("Hips"));
                    Entities.editEntity(dopplegangerID, {
                        position: Vec3.subtract(avatar.position, modelHips),
                        scale: 1.0
                    });

                    greyEntity(dopplegangerID, 1.0, lifetime);

                    return;
                }
                Script.setTimeout(setJoints, 80);
            };
            setJoints();
        }
    }

    return newEntities;
}


module.exports = {
    getTopMaterial: getTopMaterial,
    fadeAvatar: fadeAvatar,
    freezeAvatar: freezeAvatar
};
