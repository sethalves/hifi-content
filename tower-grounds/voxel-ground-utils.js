
var plotSize = 256;

floorVector = function (v) {
    return {x: Math.floor(v.x), y: Math.floor(v.y), z: Math.floor(v.z)};
}

getPlotSize = function () {
    return plotSize;
}

getTerrainAlignedLocation = function (pos) {
    var posDiv16 = Vec3.multiply(pos, 1.0 / 16.0);
    var posDiv16Floored = floorVector(posDiv16);
    return Vec3.multiply(posDiv16Floored, 16.0);
}

lookupTerrainForLocation = function (pos) {
    var baseLocation = getTerrainAlignedLocation(pos);
    entitiesAtLoc = Entities.findEntities(baseLocation, 1.0);
    for (var i = 0; i < entitiesAtLoc.length; i++) {
        var id = entitiesAtLoc[i];
        var properties = Entities.getEntityProperties(id);
        if (properties.name == "terrain") {
            return id;
        }
    }

    return false;
}

clampVector = function (low, high, vec) {
    if (vec.x < low.x) {
        vec.x = low.x;
    }
    if (vec.x > high.x) {
        vec.x = high.x;
    }
    if (vec.y < low.y) {
        vec.y = low.y;
    }
    if (vec.y > high.y) {
        vec.y = high.y;
    }
    if (vec.z < low.z) {
        vec.z = low.z;
    }
    if (vec.z > high.z) {
        vec.z = high.z;
    }
}

vecHasVolume = function (vec) {
    return (vec.x > 0) && (vec.y > 0) && (vec.z > 0);
}

addTerrainAtPosition = function (position) {
    Entities.addEntity({
        type: "PolyVox",
        name: "terrain",
        position: position,
        dimensions: { x: plotSize, y: plotSize, z: plotSize },
        voxelVolumeSize: { x: 16, y: 64, z: 16 },
        voxelSurfaceStyle: 0,
        xTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg",
        yTextureURL: "http://headache.hungry.com/~seth/hifi/grass.png",
        zTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg"
    });
}
