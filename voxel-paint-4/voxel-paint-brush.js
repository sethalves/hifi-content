
/* global Entities, genericTool, Script, Vec3, Quat */

(function() {
    Script.include("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");

    var colorToUrl = {
        0: ["http://headache.hungry.com/~seth/hifi/wood.jpg",
            "http://headache.hungry.com/~seth/hifi/wood.jpg",
            "http://headache.hungry.com/~seth/hifi/wood.jpg"],
        1: ["http://headache.hungry.com/~seth/hifi/green.png",
            "http://headache.hungry.com/~seth/hifi/green.png",
            "http://headache.hungry.com/~seth/hifi/green.png"],
        2: ["http://headache.hungry.com/~seth/hifi/dirt.jpeg",
            "http://headache.hungry.com/~seth/hifi/grass.png",
            "http://headache.hungry.com/~seth/hifi/dirt.jpeg"]
    };

    var brush = genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brush, ["position", "rotation",
                                                                       "dimensions", "registrationPoint", "userData"]);
            // var editSphereRadius = 0.035;
            var editSphereRadius = brushProps.dimensions.x / 2.0;
            var color = JSON.parse(brushProps.userData).color;

            var ids = this.addPolyVoxIfNeeded(brushProps.position, editSphereRadius, color);

            for (var i = 0; i < ids.length; i++) {
                Entities.setVoxelSphere(ids[i], brushProps.position, editSphereRadius, 255);
            }
        },
        null); // stop

    brush.slices = 10;
    brush.voxelSize = 16;

    brush.getPolyVox = function (x, y, z, c) {
        if (!this.polyvoxes) {
            return null;
        }
        if (!this.polyvoxes[x]) {
            return null;
        }
        if (!this.polyvoxes[x][y]) {
            return null;
        }
        if (!this.polyvoxes[x][y][z]) {
            return null;
        }
        return this.polyvoxes[x][y][z][c];
    };


    brush.linkToNeighbors = function (x, y, z, c) {
        if (x < 0 || x >= this.slices ||
            y < 0 || y >= this.slices ||
            z < 0 || z >= this.slices) {
            return;
        }

        // link all the polyvoxes to their neighbors
        var polyvox = this.getPolyVox(x, y, z, c);
        if (polyvox) {
            var neighborProperties = {};
            if (x > 0) {
                var xNNeighborID = this.getPolyVox(x - 1, y, z, c);
                if (xNNeighborID) {
                    neighborProperties.xNNeighborID = xNNeighborID;
                }
            }
            if (x < this.slices - 1) {
                var xPNeighborID = this.getPolyVox(x + 1, y, z, c);
                if (xPNeighborID) {
                    neighborProperties.xPNeighborID = xPNeighborID;
                }
            }
            if (y > 0) {
                var yNNeighborID = this.getPolyVox(x, y - 1, z, c);
                if (yNNeighborID) {
                    neighborProperties.yNNeighborID = yNNeighborID;
                }
            }
            if (y < this.slices - 1) {
                var yPNeighborID = this.getPolyVox(x, y + 1, z, c);
                if (yPNeighborID) {
                    neighborProperties.yPNeighborID = yPNeighborID;
                }
            }
            if (z > 0) {
                var zNNeighborID = this.getPolyVox(x, y, z - 1, c);
                if (zNNeighborID) {
                    neighborProperties.zNNeighborID = zNNeighborID;
                }
            }
            if (z < this.slices - 1) {
                var zPNeighborID = this.getPolyVox(x, y, z + 1, c);
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


    brush.addPolyVoxIfNeeded = function (brushPosition, editSphereRadius, color) {
        // find all nearby entities
        var searchRadius = 3.0;

        var ids = Entities.findEntities(brushPosition, searchRadius);

        // gather properties
        var props = {};
        for (var i = 0; i < ids.length; i++) {
            var nearbyID = ids[i];
            props[nearbyID] = Entities.getEntityProperties(nearbyID,
                                                           ['type', 'name', 'localPosition',
                                                            'position', 'rotation', 'dimensions', 'userData']);
        }

        // find the base-plate and aether
        var platformID = null;
        var aetherID = null;
        for (i = 0; i < ids.length; i++) {
            var possiblePlatOrAetherID = ids[i];
            if (props[possiblePlatOrAetherID].name == "voxel paint floor") {
                platformID = possiblePlatOrAetherID;
            }
            if (props[possiblePlatOrAetherID].name == "voxel paint aether") {
                aetherID = possiblePlatOrAetherID;
            }
            if (platformID && aetherID) {
                break;
            }
        }

        if (!platformID) {
            return ids;
        }

        if (!aetherID) {
            return ids;
        }

        var aetherProps = props[aetherID];
        var aetherDimensions = aetherProps.dimensions;
        var aetherHalfDimensions = Vec3.multiply(aetherDimensions, 0.5);
        var sliceSize = Vec3.multiply(aetherDimensions, 1.0 / this.slices);
        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);

        // find all the current polyvox entities
        var withThisColorIDs = [];
        this.polyvoxes = {};
        for (i = 0; i < ids.length; i++) {
            var possiblePolyVoxID = ids[i];
            if (props[possiblePolyVoxID].name != "voxel paint") {
                continue;
            }
            var userData = JSON.parse(props[possiblePolyVoxID].userData);
            var cFind = userData.color;

            if (cFind != color) {
                continue;
            }

            var centerOffset = Vec3.sum(props[possiblePolyVoxID].localPosition, aetherHalfDimensions);
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
            if (!this.polyvoxes[xFind][yFind][zFind]) {
                this.polyvoxes[xFind][yFind][zFind] = {};
            }
            this.polyvoxes[xFind][yFind][zFind][cFind] = possiblePolyVoxID;
            withThisColorIDs.push(possiblePolyVoxID);
        }

        var brushOffset = Vec3.multiplyQbyV(Quat.inverse(aetherProps.rotation),
                                            Vec3.subtract(brushPosition, aetherProps.position));
        brushOffset = Vec3.sum(brushOffset, Vec3.multiply(aetherProps.dimensions, 0.5));
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
                        var newID = this.addPolyVox(x, y, z, color, sliceSize, aetherID, aetherProps.dimensions);
                        if (newID) {
                            withThisColorIDs.push(newID);
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
                this.linkToNeighbors(nX, nY, nZ, color);
            }
        }

        return withThisColorIDs;
    };


    brush.addPolyVox = function (x, y, z, c, sliceSize, aetherID, aetherDimensions) {
        if (!this.polyvoxes[x]) {
            this.polyvoxes[x] = {};
        }
        if (!this.polyvoxes[x][y]) {
            this.polyvoxes[x][y] = {};
        }
        if (!this.polyvoxes[x][y][z]) {
            this.polyvoxes[x][y][z] = {};
        }
        if (this.polyvoxes[x][y][z][c]) {
            return null;
        }

        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);

        var localPosition = Vec3.sum({x: x * sliceSize.x, y: y * sliceSize.y, z: z * sliceSize.z}, halfSliceSize);
        localPosition = Vec3.subtract(localPosition, Vec3.multiply(aetherDimensions, 0.5));

        this.polyvoxes[x][y][z][c] = Entities.addEntity({
            type: "PolyVox",
            name: "voxel paint",
            // offset each color slightly to try to avoid z-buffer fighting
            localPosition: Vec3.sum(localPosition, Vec3.multiply({x: c, y: c, z: c}, 0.002)),
            dimensions: sliceSize,
            voxelVolumeSize: { x: this.voxelSize, y: this.voxelSize, z: this.voxelSize },
            voxelSurfaceStyle: 0,
            collisionless: true,
            lifetime: 28800.0, // 8 hours
            xTextureURL: colorToUrl[c][0],
            yTextureURL: colorToUrl[c][1],
            zTextureURL: colorToUrl[c][2],
            userData: JSON.stringify({color: c}),
            parentID: aetherID
        });

        // Entities.addEntity({
        //     name: "voxel paint debug cube",
        //     type: "Model",
        //     modelURL: "http://headache.hungry.com/~seth/hifi/voxel-paint-4/unitBoxTransparent.fbx",
        //     localPosition: localPosition,
        //     dimensions: sliceSize,
        //     collisionless: true,
        //     // lifetime: 60.0
        //     parentID: aetherID
        // });

        return this.polyvoxes[x][y][z][c];
    };

    return brush;
});
