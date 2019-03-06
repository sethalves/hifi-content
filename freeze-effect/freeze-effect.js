
/* global module, Graphics, Entities, Vec3, Quat */

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

function fadeAvatar(avatarID, opacity, lifetime) {
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
                            materials: {
                                model: "hifi_pbr",
                                opacity: opacity,
                                defaultFallthrough: true
                            }
                        }),
                        localPosition: Vec3.ZERO,
                        localRotation: Quat.IDENTITY,
                        parentID: avatarID,
                        priority: topMaterial.priority + 1,
                        parentMaterialName: m.toString(),
                        lifetime: lifetime
                    });

                    newEntities["material-" + m] = materialID;
                }
            }
        }
    }

    return newEntities;
}

function freezeAvatar(avatarID, lifetime) {
    // Script.setTimeout(function () {
    // });
    var newEntities = fadeAvatar(avatarID, lifetime);
    return newEntities;
}


module.exports = {
    getTopMaterial: getTopMaterial,
    fadeAvatar: fadeAvatar,
    freezeAvatar: freezeAvatar
};
