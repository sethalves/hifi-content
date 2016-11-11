
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
            var ids = this.addPolyVoxIfNeeded(brushProps.position);

            var editSphereRadius = 0.035;
            for (var i = 0; i < ids.length; i++) {
                Entities.setVoxelSphere(ids[i], brushProps.position, editSphereRadius, 255);
            }
        },
        null); // stop



    brush.slices = 5;
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


    brush.addPolyVoxIfNeeded = function (brushPosition) {
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
            return;
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
            var x = Math.round(lowCornerOffset.x / sliceSize.x);
            var y = Math.round(lowCornerOffset.y / sliceSize.y);
            var z = Math.round(lowCornerOffset.z / sliceSize.z);
            if (!this.polyvoxes[x]) {
                this.polyvoxes[x] = {};
            }
            if (!this.polyvoxes[x][y]) {
                this.polyvoxes[x][y] = {};
            }
            this.polyvoxes[x][y][z] = possiblePolyVoxID;
        }

        var brushOffset = Vec3.subtract(brushPosition, platformCorner);
        var brushRatio = { x: brushOffset.x / sliceSize.x,
                           y: brushOffset.y / sliceSize.y,
                           z: brushOffset.z / sliceSize.z };
        var brushRatioCenter = Vec3.subtract(brushRatio, {x:0.5, y:0.5, z:0.5});
        var brushIndex = { x: Math.round(brushRatioCenter.x),
                           y: Math.round(brushRatioCenter.y),
                           z: Math.round(brushRatioCenter.z) };
        this.addPolyVox(brushIndex.x, brushIndex.y, brushIndex.z, platformCorner, sliceSize);

        return ids;
    };


    brush.addPolyVox = function (x, y, z, platformCorner, sliceSize) {
        if (x < 0 || x >= this.slices ||
            y < 0 || y >= this.slices ||
            z < 0 || z >= this.slices) {
            return;
        }
        if (!this.polyvoxes[x]) {
            this.polyvoxes[x] = {};
        }
        if (!this.polyvoxes[x][y]) {
            this.polyvoxes[x][y] = {};
        }
        if (this.polyvoxes[x][y][z]) {
            return;
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

        this.linkToNeighbors(x, y, z);
        this.linkToNeighbors(x - 1, y, z);
        this.linkToNeighbors(x + 1, y, z);
        this.linkToNeighbors(x, y + 1, z);
        this.linkToNeighbors(x, y - 1, z);
        this.linkToNeighbors(x, y, z - 1);
        this.linkToNeighbors(x, y, z + 1);

        // for (var debugX = 0; debugX < this.voxelSize - 1; debugX++) {
        //     Entities.setVoxel(this.polyvoxes[x][y][z], {x:debugX, y:0, z:0}, 255);
        //     Entities.setVoxel(this.polyvoxes[x][y][z], {x:debugX, y:this.voxelSize - 1, z:0}, 255);
        //     Entities.setVoxel(this.polyvoxes[x][y][z], {x:debugX, y:0, z:this.voxelSize - 1}, 255);
        //     Entities.setVoxel(this.polyvoxes[x][y][z], {x:debugX, y:this.voxelSize - 1, z:this.voxelSize - 1}, 255);
        // }
    };

    return brush;
});
