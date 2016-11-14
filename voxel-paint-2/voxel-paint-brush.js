
/* global Entities, genericTool, Script, Vec3 */

(function() {
    Script.include("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");

    var brush = genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brush, ["position", "rotation",
                                                                       "dimensions", "registrationPoint"]);
            // var editSphereRadius = 0.035;
            var editSphereRadius = brushProps.dimensions.x / 2.0;
            var ids = this.addPolyVoxIfNeeded(brushProps.position, editSphereRadius);

            for (var i = 0; i < ids.length; i++) {
                Entities.setVoxelSphere(ids[i], brushProps.position, editSphereRadius, 255);
            }
        },
        null); // stop



    brush.slices = 10;
    brush.voxelSize = 16;

    brush.getPolyVox = function (x, y, z) {
        if (!this.polyvoxes) {
            return null;
        }
        if (!this.polyvoxes[x]) {
            return null;
        }
        if (!this.polyvoxes[x][y]) {
            return null;
        }
        return this.polyvoxes[x][y][z];
    };


    brush.linkToNeighbors = function (x, y, z) {
        if (x < 0 || x >= this.slices ||
            y < 0 || y >= this.slices ||
            z < 0 || z >= this.slices) {
            return null;
        }

        // link all the polyvoxes to their neighbors
        var polyvox = this.getPolyVox(x, y, z);
        if (polyvox) {
            var neighborProperties = {};
            if (x > 0) {
                var xNNeighborID = this.getPolyVox(x - 1, y, z);
                if (xNNeighborID) {
                    neighborProperties.xNNeighborID = xNNeighborID;
                }
            }
            if (x < this.slices - 1) {
                var xPNeighborID = this.getPolyVox(x + 1, y, z);
                if (xPNeighborID) {
                    neighborProperties.xPNeighborID = xPNeighborID;
                }
            }
            if (y > 0) {
                var yNNeighborID = this.getPolyVox(x, y - 1, z);
                if (yNNeighborID) {
                    neighborProperties.yNNeighborID = yNNeighborID;
                }
            }
            if (y < this.slices - 1) {
                var yPNeighborID = this.getPolyVox(x, y + 1, z);
                if (yPNeighborID) {
                    neighborProperties.yPNeighborID = yPNeighborID;
                }
            }
            if (z > 0) {
                var zNNeighborID = this.getPolyVox(x, y, z - 1);
                if (zNNeighborID) {
                    neighborProperties.zNNeighborID = zNNeighborID;
                }
            }
            if (z < this.slices - 1) {
                var zPNeighborID = this.getPolyVox(x, y, z + 1);
                if (zPNeighborID) {
                    neighborProperties.zPNeighborID = zPNeighborID;
                }
            }
            Entities.editEntity(polyvox, neighborProperties);
        }
    };


    brush.clamp = function (v) {
        return Math.min(Math.max(v, 0), this.slices - 1);
    };


    brush.addPolyVoxIfNeeded = function (brushPosition, editSphereRadius) {
        // find all nearby entities
        var searchRadius = 3.0;

        var ids = Entities.findEntities(brushPosition, searchRadius);

        // gather properties
        var props = {};
        for (var i = 0; i < ids.length; i++) {
            var nearbyID = ids[i];
            props[nearbyID] = Entities.getEntityProperties(nearbyID, ['name', 'position', 'dimensions']);
        }

        // find the base-plate
        var platformID = null;
        for (i = 0; i < ids.length; i++) {
            var possiblePlatformID = ids[i];
            if (props[possiblePlatformID].name == "voxel paint floor") {
                platformID = possiblePlatformID;
                break;
            }
        }

        if (!platformID) {
            return ids;
        }

        var platformProps = props[platformID];
        var platformDimensions = platformProps.dimensions;
        var platformHalfDimensions = Vec3.multiply(platformDimensions, 0.5);
        var platformCorner = Vec3.subtract(platformProps.position, platformHalfDimensions);
        platformDimensions.y = platformDimensions.x;
        var sliceSize = Vec3.multiply(platformDimensions, 1.0 / this.slices);
        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);

        // find all the current polyvox entities
        this.polyvoxes = {};
        for (i = 0; i < ids.length; i++) {
            var possiblePolyVoxID = ids[i];
            if (props[possiblePolyVoxID].name != "voxel paint") {
                continue;
            }
            var centerOffset = Vec3.subtract(props[possiblePolyVoxID].position, platformCorner);
            var lowCornerOffset = Vec3.subtract(centerOffset, halfSliceSize);
            var xFind = Math.round(lowCornerOffset.x / sliceSize.x);
            var yFind = Math.round(lowCornerOffset.y / sliceSize.y);
            var zFind = Math.round(lowCornerOffset.z / sliceSize.z);
            if (!this.polyvoxes[xFind]) {
                this.polyvoxes[xFind] = {};
            }
            if (!this.polyvoxes[xFind][yFind]) {
                this.polyvoxes[xFind][yFind] = {};
            }
            this.polyvoxes[xFind][yFind][zFind] = possiblePolyVoxID;
        }

        var brushOffset = Vec3.subtract(brushPosition, platformCorner);
        var brushPosInVoxSpace = { x: brushOffset.x / sliceSize.x,
                           y: brushOffset.y / sliceSize.y,
                           z: brushOffset.z / sliceSize.z };

        var radiusInVoxelSpace = editSphereRadius / sliceSize.x;
        var sliceHalfDiag = 0.866; // Vec3.length({x:0.5, y:0.5, z:0.5});
        var sliceRezDistance = radiusInVoxelSpace + sliceHalfDiag;

        var lowX = this.clamp(Math.floor(brushPosInVoxSpace.x - sliceRezDistance));
        var highX = this.clamp(Math.ceil(brushPosInVoxSpace.x + sliceRezDistance));
        var lowY = this.clamp(Math.floor(brushPosInVoxSpace.y - sliceRezDistance));
        var highY = this.clamp(Math.ceil(brushPosInVoxSpace.y + sliceRezDistance));
        var lowZ = this.clamp(Math.floor(brushPosInVoxSpace.z - sliceRezDistance));
        var highZ = this.clamp(Math.ceil(brushPosInVoxSpace.z + sliceRezDistance));

        this.dirtyNeighbors = {};

        for (var x = lowX; x < highX; x++) {
            for (var y = lowY; y < highY; y++) {
                for (var z = lowZ; z < highZ; z++) {
                    if (Vec3.distance(Vec3.sum({x:x, y:y, z:z}, {x:0.5, y:0.5, z:0.5}),
                                      brushPosInVoxSpace) < sliceRezDistance) {
                    // if (Vec3.distance(Vec3.sum({x:x, y:y, z:z}, halfSliceSize), brushPosInVoxSpace) < sliceRezDistance) {
                        var newID = this.addPolyVox(x, y, z, platformCorner, sliceSize);
                        if (newID) {
                            ids.push(newID);
                            // keep track of which PolyVoxes need their neighbors hooked up
                            for (var dx = -1; dx <= 1; dx++) {
                                for (var dy = -1; dy <= 1; dy++) {
                                    for (var dz = -1; dz <= 1; dz++) {
                                        this.dirtyNeighbors["" + (x+dx) + "," + (y+dy) + "," + (z+dz)] = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        for (var neighborKey in this.dirtyNeighbors) {
            if (this.dirtyNeighbors.hasOwnProperty(neighborKey)) {
                var xyz = neighborKey.split(",");
                var nX = parseInt(xyz[0]);
                var nY = parseInt(xyz[1]);
                var nZ = parseInt(xyz[2]);
                print("linking to neighbors of " + nX + " " + nY + " " + nZ);
                this.linkToNeighbors(nX, nY, nZ);
            }
        }

        return ids;
    };


    brush.addPolyVox = function (x, y, z, platformCorner, sliceSize) {
        if (!this.polyvoxes[x]) {
            this.polyvoxes[x] = {};
        }
        if (!this.polyvoxes[x][y]) {
            this.polyvoxes[x][y] = {};
        }
        if (this.polyvoxes[x][y][z]) {
            return null;
        }

        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);
        var position = Vec3.sum({x: platformCorner.x + (x * sliceSize.x),
                                 y: platformCorner.y + (y * sliceSize.y),
                                 z: platformCorner.z + (z * sliceSize.z)},
                                halfSliceSize);
        this.polyvoxes[x][y][z] = Entities.addEntity({
            type: "PolyVox",
            name: "voxel paint",
            position: position,
            dimensions: sliceSize,
            voxelVolumeSize: { x: this.voxelSize, y: this.voxelSize, z: this.voxelSize },
            voxelSurfaceStyle: 0,
            collisionless: true,
            lifetime: 28800.0, // 8 hours
            xTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg",
            yTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg",
            zTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg"
        });

        Entities.addEntity({
            name: "voxel paint debug cube",
            type: "Model",
            modelURL: "http://headache.hungry.com/~seth/hifi/voxel-paint-2/unitBoxTransparent.fbx",
            position: position,
            dimensions: sliceSize,
            collisionless: true,
            // lifetime: 60.0
        });

        return this.polyvoxes[x][y][z];
    };

    return brush;
});
