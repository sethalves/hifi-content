
/* global Entities, genericTool, Script, Vec3, Quat, textureIndexToURLs, paintBucketColors */

(function() {
    Script.include("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    Script.include(Script.resolvePath('voxel-paint-shared.js'));

    var brush = genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brush, ["position", "rotation",
                                                                       "dimensions", "registrationPoint", "userData"]);
            // var editSphereRadius = 0.035;
            var editSphereRadius = brushProps.dimensions.x / 2.0;
            this.color = JSON.parse(brushProps.userData).color;

            var ids = this.addPolyVoxIfNeeded(this.previousBrushPosition, brushProps.position, editSphereRadius);

            for (var i = 0; i < ids.length; i++) {
                Entities.setVoxelCapsule(ids[i], this.previousBrushPosition, brushProps.position, editSphereRadius, 255);
            }

            this.previousBrushPosition = brushProps.position;
        },
        null); // stop

    brush.slices = 10;
    brush.voxelSize = 16;
    brush.showPolyVoxes = true;


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


    brush.addPolyVoxIfNeeded = function (previousBrushPosition, brushPosition, editSphereRadius) {
        if (!this.aetherID) {
            var aetherSearchRadius = 3.0;
            var possibleAetherIDs = Entities.findEntities(brushPosition, aetherSearchRadius);
            for (var j = 0; j < possibleAetherIDs.length; j++) {
                var possibleAetherID = possibleAetherIDs[j];
                var possibleAetherProps = Entities.getEntityProperties(possibleAetherID, ['name']);
                if (possibleAetherProps.name == "voxel paint aether") {
                    this.aetherID = possibleAetherID;
                    this.aetherProps = Entities.getEntityProperties(this.aetherID, ['position', 'rotation', 'dimensions']);
                    break;
                }
            }
        }

        if (!this.aetherID) {
            print("error -- voxel-paint-tool can't find aether");
            return Entities.findEntities(brushPosition, editSphereRadius);
        }

        var aetherDimensions = this.aetherProps.dimensions;
        var aetherHalfDimensions = Vec3.multiply(aetherDimensions, 0.5);
        var sliceSize = Vec3.multiply(aetherDimensions, 1.0 / this.slices);
        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);
        var sliceRadius = Math.sqrt(Vec3.dot(halfSliceSize, halfSliceSize));
        var capsuleLength = 2 * editSphereRadius + Vec3.length(Vec3.subtract(brushPosition, previousBrushPosition));
        var capsuleCenter = Vec3.multiply(Vec3.sum(brushPosition, previousBrushPosition), 0.5);

        var ids = Entities.findEntities(capsuleCenter, capsuleLength + sliceRadius + 0.05);

        // find all the current polyvox entities
        var withThisColorIDs = [];
        this.polyvoxes = {};
        for (var i = 0; i < ids.length; i++) {
            var possiblePolyVoxID = ids[i];

            var props;
            if (possiblePolyVoxID in this.propsCache) {
                props = this.propsCache[possiblePolyVoxID];
            } else {
                props = Entities.getEntityProperties(possiblePolyVoxID, ['name', 'localPosition', 'userData']);
                if (props.name != "voxel paint") {
                    continue;
                }
                this.propsCache[possiblePolyVoxID] = props;
            }

            // check for paint-buckets
            if (props.name.substring(0, 25) == "voxel paint color bucket " &&
                Vec3.distance(brushPosition, props.position) < props.dimensions.x) {
                var newColorIndex = JSON.parse(props.userData).color;
                if (newColorIndex != this.color) {
                    this.color = newColorIndex;
                    print("switch to color: " + newColorIndex);
                    Entities.editEntity(this.brush, { color: paintBucketColors[ newColorIndex ],
                                                      userData: JSON.stringify({color: newColorIndex})});
                }
            }

            var userData = JSON.parse(props.userData);
            var cFind = userData.color;

            if (cFind != this.color) {
                continue;
            }

            var centerOffset = Vec3.sum(props.localPosition, aetherHalfDimensions);
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

        var previousBrushOffset = Vec3.multiplyQbyV(Quat.inverse(this.aetherProps.rotation),
                                                    Vec3.subtract(this.previousBrushPosition,
                                                                  this.aetherProps.position));
        previousBrushOffset = Vec3.sum(previousBrushOffset, Vec3.multiply(aetherDimensions, 0.5));
        var previousBrushPosInVoxSpace = { x: previousBrushOffset.x / sliceSize.x,
                                           y: previousBrushOffset.y / sliceSize.y,
                                           z: previousBrushOffset.z / sliceSize.z };


        var brushOffset = Vec3.multiplyQbyV(Quat.inverse(this.aetherProps.rotation),
                                            Vec3.subtract(brushPosition, this.aetherProps.position));
        brushOffset = Vec3.sum(brushOffset, Vec3.multiply(aetherDimensions, 0.5));
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

        lowX = Math.min(lowX, this.clamp(Math.floor(previousBrushPosInVoxSpace.x - sliceRezDistance)));
        highX = Math.max(highX, this.clamp(Math.ceil(previousBrushPosInVoxSpace.x + sliceRezDistance)));
        lowY = Math.min(lowY, this.clamp(Math.floor(previousBrushPosInVoxSpace.y - sliceRezDistance)));
        highY = Math.max(highY, this.clamp(Math.ceil(previousBrushPosInVoxSpace.y + sliceRezDistance)));
        lowZ = Math.min(lowZ, this.clamp(Math.floor(previousBrushPosInVoxSpace.z - sliceRezDistance)));
        highZ = Math.max(highZ, this.clamp(Math.ceil(previousBrushPosInVoxSpace.z + sliceRezDistance)));

        this.dirtyNeighbors = {};

        for (var x = lowX; x < highX; x++) {
            for (var y = lowY; y < highY; y++) {
                for (var z = lowZ; z < highZ; z++) {
                    var touches;
                    touches = Entities.AABoxIntersectsCapsule({x:x, y:y, z:z}, {x:1.0, y:1.0, z:1.0},
                                                              previousBrushPosInVoxSpace, brushPosInVoxSpace,
                                                              radiusInVoxelSpace);
                    if (touches) {
                        var newID = this.addPolyVox(x, y, z, this.color, sliceSize, aetherDimensions);
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
                this.linkToNeighbors(nX, nY, nZ, this.color);
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
            // we already have a polyvox for this position and color
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
            // lifetime: 28800.0, // 8 hours
            xTextureURL: textureIndexToURLs[c][0],
            yTextureURL: textureIndexToURLs[c][1],
            zTextureURL: textureIndexToURLs[c][2],
            userData: JSON.stringify({color: c}),
            parentID: aetherID
        });

        if (this.showPolyVoxes) {
            Entities.addEntity({
                name: "voxel paint debug cube",
                type: "Model",
                modelURL: "http://headache.hungry.com/~seth/hifi/voxel-paint-4/unitBoxTransparent.fbx",
                localPosition: localPosition,
                dimensions: sliceSize,
                collisionless: true,
                // lifetime: 60.0
                parentID: aetherID
            });
        }

        return this.polyvoxes[x][y][z][c];
    };

    return brush;
});
