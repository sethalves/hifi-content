
/* global module, Graphics, Entities, Vec3, Quat, AvatarList, Script, AvatarManager */

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
    var newEntityIDs = {};
    var mesh = Graphics.getModel(avatarID);
    if (mesh) {
        var materials = mesh.materialLayers;
        for (var m in materials) {
            if (materials.hasOwnProperty(m) && parseInt(m.toString()) == m) {
                var multiMaterial = materials[m];
                if (multiMaterial) {
                    var topMaterial = getTopMaterial(multiMaterial);
                    var materialEntityID = Entities.addEntity({
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

                    newEntityIDs["material-" + m + "-" + avatarID] = materialEntityID;
                }
            }
        }
    }

    return newEntityIDs;
}


function fadeTarget(avatarID, opacity, lifetime) {
    return applyMaterial(avatarID, opacity, lifetime, {
        model: "hifi_pbr",
        opacity: opacity,
        defaultFallthrough: true
    });
}


function colorTarget(entityID, opacity, lifetime, color) {
    return applyMaterial(entityID, opacity, lifetime, {
        albedo: color,
        opacity: opacity,
        defaultFallthrough: true
    });
}


function scheduleUnfreeze(avatarID, position, rotation, lifetime, newEntityIDs) {
    Script.setTimeout(function () {
        print("QQQQ unfreezing avatar " + JSON.stringify(avatarID));
        for (var entityKey in newEntityIDs) {
            if (newEntityIDs.hasOwnProperty(entityKey)) {
                Entities.deleteEntity(newEntityIDs[entityKey]);
            }
        }
    }, lifetime * 1000);
}


function addLockdownEntity(avatarID, position, rotation, lifetime) {
    Entities.addEntity({
        name: "dead",
        type: "Sphere",
        color: { red: 0, green: 0, blue: 0 },
        dimensions: 0.1,
        localPosition: Vec3.ZERO,
        dynamic: false,
        collisionless: true,
        lifetime: lifetime,
        alpha: 0.0,
        parentID: avatarID,
        script: Script.resolvePath("lockdown.js"),
        userData: JSON.stringify({
            position: position,
            rotation: rotation,
            lifetime: lifetime
        }),
        ignorePickIntersection: true,
        grab: { grabbable: false }
    });
}


function freezeAvatar(avatarID, lifetime) {
    print("QQQQ freezing avatar " + JSON.stringify(avatarID) + " for " + lifetime + " seconds.");
    var newEntityIDs = fadeTarget(avatarID, 0.0, lifetime + 2);

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

            var position = avatar.position;
            var rotation = avatar.orientation;

            var dopplegangerID = Entities.addEntity({
                type: "Model",
                name: "frozen avatar: " + avatar.displayName,
                position: position,
                // position: hipsPosition,
                // position: Vec3.sum(avatar.position, offset),
                rotation: Quat.multiply(Quat.fromPitchYawRollDegrees(0, 180, 0), rotation),
                // rotation: Quat.fromPitchYawRollRadians(0, avatar.bodyYaw, 0),
                // avatar.scale
                // dimensions: { x: 0.5, y: 0.5, z: 0.5 },
                collisionless: true,
                dynamic: 0,
                modelURL: modelURL,
                lifetime: lifetime + 2
                // jointRotations: jointRotations,
                // jointRotationsSet: jointRotationsSet
            });
            newEntityIDs.doppleganger = dopplegangerID;

            addLockdownEntity(avatarID, position, rotation, lifetime);

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
                    var props = Entities.getEntityProperties(dopplegangerID, ["naturalDimensions"]);

                    Entities.editEntity(dopplegangerID, {
                        position: Vec3.subtract(avatar.position, modelHips),
                        dimensions: props.naturalDimensions
                    });

                    var moreIDs = colorTarget(dopplegangerID, 1.0, lifetime + 2, [0.0, 0.0, 1.0]);
                    for (var entityKey in moreIDs) {
                        if (moreIDs.hasOwnProperty(entityKey)) {
                            newEntityIDs[entityKey] = moreIDs[entityKey];
                        }
                    }

                    scheduleUnfreeze(avatarID, position, rotation, lifetime, newEntityIDs);

                    return;
                }
                Script.setTimeout(setJoints, 80);
            };
            setJoints();
        }
    }

    return newEntityIDs;
}


module.exports = {
    getTopMaterial: getTopMaterial,
    fadeTarget: fadeTarget,
    colorTarget: colorTarget,
    freezeAvatar: freezeAvatar
};
