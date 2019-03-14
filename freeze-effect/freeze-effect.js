
/* global module, Graphics, Entities, Vec3, Quat, AvatarList, Script, Messages */

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


function mergeIntoAssociativeArray(dst, other) {
    for (var entityKey in other) {
        if (other.hasOwnProperty(entityKey)) {
            dst[entityKey] = other[entityKey];
        }
    }
    return dst;
}


function applyMaterial(targetID, opacity, lifetime, andChildren, materialsDict) {
    var newEntityIDs = {};

    var mesh = null;
    try {
        mesh = Graphics.getModel(targetID);
    } catch (e) {
        // print("QQQQ no mesh for " + targetID);
    }

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
                        parentID: targetID,
                        priority: topMaterial.priority + 1,
                        parentMaterialName: m.toString(),
                        lifetime: lifetime
                    }, "avatar");

                    newEntityIDs["material-" + m + "-" + targetID] = materialEntityID;
                }
            }
        }
    }

    if (andChildren) {
        var childrenIDs = Entities.getChildrenIDs(targetID);
        for (var c = 0; c < childrenIDs.length; c++) {
            var childID = childrenIDs[c];
            var moreIDs = applyMaterial(childID, opacity, lifetime, andChildren, materialsDict);
            mergeIntoAssociativeArray(newEntityIDs, moreIDs);
        }
    }

    return newEntityIDs;
}


function fadeTarget(targetID, opacity, lifetime, andChildren) {
    return applyMaterial(targetID, opacity, lifetime, andChildren, {
        model: "hifi_pbr",
        opacity: opacity,
        defaultFallthrough: true
    });
}


function colorTarget(targetID, opacity, lifetime, color, andChildren) {
    return applyMaterial(targetID, opacity, lifetime, andChildren, {
        albedo: color,
        opacity: opacity,
        defaultFallthrough: true
    });
}


function scheduleUnfreeze(avatarID, position, rotation, lifetime, newEntityIDs) {
    Script.setTimeout(function () {
        print("QQQQ unfreezing avatar " + JSON.stringify(avatarID));

        Messages.sendMessage("Freeze-Avatar", JSON.stringify({
            method: "unfreeze",
            targetID: avatarID
        }));

        for (var entityKey in newEntityIDs) {
            if (newEntityIDs.hasOwnProperty(entityKey)) {
                Entities.deleteEntity(newEntityIDs[entityKey]);
            }
        }
    }, lifetime * 1000);
}


function addLockdownEntity(avatarID, position, rotation, lifetime) {
    Entities.addEntity({
        name: "frozen-" + avatarID,
        type: "Box",
        color: { red: 0, green: 0, blue: 0 },
        dimensions: 0.1,
        localPosition: Vec3.ZERO,
        dynamic: false,
        collisionless: true,
        lifetime: lifetime + 2,
        alpha: 0.0,
        parentID: avatarID,
        script: Script.resolvePath("lockdown.js"),
        userData: JSON.stringify({
            position: position,
            rotation: rotation,
            lifetime: lifetime - 0.8 // jump target back slightly before they unfreeze
        }),
        ignorePickIntersection: true,
        grab: { grabbable: false }
    }, "avatar");
}

function isAvatarFrozen(avatarID) {
    var childrenIDs = Entities.getChildrenIDs(avatarID);
    for (var c = 0; c < childrenIDs.length; c++) {
        var props = Entities.getEntityProperties(childrenIDs[c], ["name"]);
        if (props && props.name == "frozen-" + avatarID) {
            return true;
        }
    }
    return false;
}

function freezeAvatar(avatarID, lifetime) {
    print("QQQQ freezing avatar " + JSON.stringify(avatarID) + " for " + lifetime + " seconds.");
    var newEntityIDs = fadeTarget(avatarID, 0.0, lifetime + 2, true);

    Messages.sendMessage("Freeze-Avatar", JSON.stringify({
        method: "freeze",
        targetID: avatarID
    }));

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
            }, "avatar");
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

                    var moreIDs = colorTarget(dopplegangerID, 1.0, lifetime + 2, [0.0, 0.0, 1.0], true);
                    mergeIntoAssociativeArray(newEntityIDs, moreIDs);

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
    isAvatarFrozen: isAvatarFrozen,
    freezeAvatar: freezeAvatar
};
