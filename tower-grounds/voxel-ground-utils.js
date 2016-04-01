
floorVector = function (v) {
    return {x: Math.floor(v.x), y: Math.floor(v.y), z: Math.floor(v.z)};
}

getTerrainAlignedLocation = function (pos) {
    var posDiv16 = Vec3.multiply(pos, 1.0 / 16.0);
    var posDiv16Floored = floorVector(posDiv16);
    return Vec3.multiply(posDiv16Floored, 16.0);
}

lookupTerrainForLocation = function (pos, plotSize) {
    var baseLocation = getTerrainAlignedLocation(pos);
    entitiesAtLoc = Entities.findEntities(baseLocation, 1.0);
    for (var i = 0; i < entitiesAtLoc.length; i++) {
        var id = entitiesAtLoc[i];
        var properties = Entities.getEntityProperties(id, ["name", "dimensions"]);
        if (properties.name == "terrain" && properties.dimensions.x == plotSize) {
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

minVector = function(minV, newPoint) {
    return {
        x: Math.min(minV.x, newPoint.x),
        y: Math.min(minV.y, newPoint.y),
        z: Math.min(minV.z, newPoint.z)
    };
}

maxVector = function(maxV, newPoint) {
    return {
        x: Math.max(maxV.x, newPoint.x),
        y: Math.max(maxV.y, newPoint.y),
        z: Math.max(maxV.z, newPoint.z)
    };
}

vecHasVolume = function (vec) {
    return (vec.x > 0) && (vec.y > 0) && (vec.z > 0);
}

addTerrainAtPosition = function (position, plotSize) {
    return Entities.addEntity({
        type: "PolyVox",
        name: "terrain",
        position: position,
        dimensions: { x: plotSize, y: plotSize, z: plotSize },
        voxelVolumeSize: { x: 16, y: 64, z: 16 },
        voxelSurfaceStyle: 0,
        xTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png",
        // yTextureURL: "http://headache.hungry.com/~seth/hifi/green-16x16.png",
        yTextureURL: "http://headache.hungry.com/~seth/hifi/green.png",
        zTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png"
    });
}

unLockTerrain = function () {
    print("unlock");
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var props = Entities.getEntityProperties(polyVoxID, ["name", "position"]);
        if (props.name == "terrain") {
            Entities.editEntity(polyVoxID, {locked: false});
        }
    }
}

lockTerrain = function () {
    print("lock");
    nearbyEntities = Entities.findEntities(MyAvatar.position, 1000.0);
    for (voxelTarrainCandidateIndex in nearbyEntities) {
        var polyVoxID = nearbyEntities[voxelTarrainCandidateIndex];
        var props = Entities.getEntityProperties(polyVoxID, ["name", "position"]);
        if (props.name == "terrain") {
            Entities.editEntity(polyVoxID, {locked: true});
        }
    }
}
