
/* global Entities, Overlays, Script, Controller, Vec3, Quat, PALETTE_COLORS */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    Script.include(Script.resolvePath('voxel-paint-shared.js'));

    var brush = genericTool.genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
            this.previousBrushPositionSet = false;
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brush, ["position", "dimensions", "userData"]);
            var editSphereRadius = brushProps.dimensions.x / 2.0;
            this.color = JSON.parse(brushProps.userData).color;

            if (!this.previousBrushPositionSet) {
                this.previousBrushPositionSet = true;
                this.previousBrushPosition = brushProps.position;
            }

            var ids = this.addPolyVoxIfNeeded(brushProps.position, editSphereRadius);

            for (var i = 0; i < ids.length; i++) {
                if (Entities.setVoxelCapsule(ids[i], this.previousBrushPosition, brushProps.position, editSphereRadius, 255)) {
                    Entities.editEntity(ids[i], { lifetime: this.polyVoxLifetime });
                }
            }

            this.previousBrushPosition = brushProps.position;
        },
        null); // stop

    brush.voxelVolumeSize = 16;
    brush.sliceSize = {x: 0.04, y: 0.04, z: 0.04}; // dimensions of one voxel
    brush.polyVoxSize = Vec3.multiply(brush.sliceSize, brush.voxelVolumeSize); // dimensions of one polyvox entity
    brush.showPolyVoxes = false;
    brush.previousBrushPosition = null;
    brush.previousBrushPositionSet = false;
    brush.polyVoxes = {};
    brush.menuMapping = null;
    brush.menuOverlayIDs = [];
    brush.menuItemWorldPosition = [];
    brush.equipContinued = null;

    brush.polyVoxLifetime = 200;
    // brush.polyVoxLifetime = 28800; // 8 hours


    brush.showMenu = function () {
        this.menuOpen = true;
        var brushProps = Entities.getEntityProperties(this.brush, ["position", "rotation", "dimensions", "userData"]);
        var handleProps = Entities.getEntityProperties(this.entityID, ["position", "rotation", "dimensions", "userData"]);

        var colorCount = PALETTE_COLORS.length + 1; // +1 for erase

        // place colors around 3/4 of the circle
        var colorAngleStep = (360.0 * (3/4)) / (colorCount - 1);
        var up = { x: 0, y: 1, z: 0 };
        var brushVec = Vec3.normalize(Vec3.subtract(brushProps.position, handleProps.position));
        var initialMenuItemVec = Vec3.multiply(Vec3.normalize(Vec3.cross(up, brushVec)), 0.2);
        var menuRotationVec = Vec3.normalize(Vec3.cross(initialMenuItemVec, up));

        for (var menuItemIndex = 0; menuItemIndex < colorCount; menuItemIndex++) {
            var menuItemAngle = menuItemIndex * colorAngleStep;
            var menuItemQuat = Quat.angleAxis(menuItemAngle, menuRotationVec);
            var rotatedMenuItemVec = Vec3.multiplyQbyV(menuItemQuat, initialMenuItemVec);
            var menuItemWorldPos = Vec3.sum(brushProps.position, rotatedMenuItemVec);

            if (menuItemIndex < PALETTE_COLORS.length) {
                var colorOverlayID = Overlays.addOverlay("sphere", {
                    position: menuItemWorldPos,
                    alpha: 0.9,
                    dimensions: 0.06,
                    solid: true,
                    color: PALETTE_COLORS[menuItemIndex].color,
                    ignoreRayIntersection: true
                });

                this.menuOverlayIDs.push(colorOverlayID);
            }
            this.menuItemWorldPosition.push(menuItemWorldPos);
        }

        this.equipContinued = function (id, params) {
            var brushProps = Entities.getEntityProperties(brush.brush, ["position", "rotation", "dimensions", "userData"]);
            for (var j = 0; j < brush.menuItemWorldPosition.length; j++) {
                var distanceFromMenuItem = Vec3.distance(brushProps.position, brush.menuItemWorldPosition[j]);
                if (distanceFromMenuItem < 0.03) {
                    var brushUserData = JSON.parse(brushProps.userData);
                    brushUserData.color = j;
                    Entities.editEntity(brush.brush, {
                        userData: JSON.stringify(brushUserData),
                        color: PALETTE_COLORS[j].color
                    });
                }
            }
        };
    };


    brush.hideMenu = function () {
        this.equipContinued = null;
        this.menuOpen = false;
        for (var i = 0; i < this.menuOverlayIDs.length; i++) {
            Overlays.deleteOverlay(this.menuOverlayIDs[i]);
        }
        this.menuOverlayIDs = [];
        this.menuItemWorldPosition = [];
    };


    brush.showOrHideMenu = function (value) {
        if (value === 1) {
            brush.showMenu();
        } else {
            brush.hideMenu();
        }
    };


    brush.equipStarted = function (id, params) {
        var mappingName = 'Voxel-Paint-Mapping-' + this.hand + "-" + Math.random();
        if (this.menuMapping) {
            this.menuMapping.disable();
        }
        this.menuMapping = Controller.newMapping(mappingName);
        if (this.hand === 0) {
            // equipped in left
            this.menuMapping.from(Controller.Standard.LeftPrimaryThumb).peek().to(brush.showOrHideMenu);
        } else {
            // equipped in right
            this.menuMapping.from(Controller.Standard.RightPrimaryThumb).peek().to(brush.showOrHideMenu);
        }
        Controller.enableMapping(mappingName);
    };


    brush.equipStopped = function () {
        this.menuMapping.disable();
        this.menuMapping = null;
        this.hideMenu();
    };


    brush.getPolyVoxProperties = function (polyVoxID) {
        var polyVoxProps = Entities.getEntityProperties(polyVoxID, ["name", "position", "userData",
                                                                    "age", "lifetime", "parentID"]);
        if (polyVoxProps.name != "voxel paint") {
            return null;
        }
        polyVoxProps.expires = Date.now() + polyVoxProps.lifetime - polyVoxProps.age;
        return polyVoxProps;
    };


    brush.addNewAether = function (worldAetherPosition) {
        var aetherID = Entities.addEntity({
            color: { red: 0, green: 0, blue: 0 },
            dimensions: { x: 1, y: 1, z: 1 },
            name: "voxel paint aether",
            position: worldAetherPosition,
            rotation: { x: 0, y: 0, z: 0, w: 1 },
            shape: "Cube",
            type: "Box",
            collidesWith: "",
            collisionMask: 0,
            collisionless: true,
            visible: false,
            userData: JSON.stringify({ grabbableKey: {grabbable: true} }),
            lifetime: this.polyVoxLifetime
        });
        return aetherID;
    };


    brush.worldPositionToPolyVoxIndex = function (brushWorldPosition, aetherProps) {
        var brushOffsetFromAether = Vec3.subtract(brushWorldPosition, aetherProps.position);
        var aetherToWorldRotation = aetherProps.rotation;
        var worldToAetherRotation = Quat.inverse(aetherToWorldRotation);
        var brushPositionInAetherFrame = Vec3.multiplyQbyV(worldToAetherRotation, brushOffsetFromAether);
        return {
            x: Math.floor(brushPositionInAetherFrame.x / this.polyVoxSize.x),
            y: Math.floor(brushPositionInAetherFrame.y / this.polyVoxSize.y),
            z: Math.floor(brushPositionInAetherFrame.z / this.polyVoxSize.z)
        };
    };


    brush.polyVoxIndextoLocalPosition = function(polyVoxIndex, aetherProps) {
        var brushPositionInAetherFrame = {
            x: polyVoxIndex.x * this.polyVoxSize.x + (this.polyVoxSize.x / 2.0),
            y: polyVoxIndex.y * this.polyVoxSize.y + (this.polyVoxSize.y / 2.0),
            z: polyVoxIndex.z * this.polyVoxSize.z + (this.polyVoxSize.z / 2.0)
        };
        return brushPositionInAetherFrame;
    };


    // brush.polyVoxIndextoWorldPosition = function(polyVoxIndex, aetherProps) {
    //     var brushPositionInAetherFrame = brush.polyVoxIndextoLocalPosition(polyVoxIndex, aetherProps);
    //     var aetherToWorldRotation = aetherProps.rotation;
    //     var brushOffsetFromAether = Vec3.multiplyQbyV(aetherToWorldRotation, brushPositionInAetherFrame);
    //     var brushWorldPosition = Vec3.sum(aetherProps.position, brushOffsetFromAether);
    //     return brushWorldPosition;
    // };


    brush.getPolyVox = function (x, y, z, c) {
        if (!this.polyVoxes[x]) {
            return null;
        }
        if (!this.polyVoxes[x][y]) {
            return null;
        }
        if (!this.polyVoxes[x][y][z]) {
            return null;
        }
        return this.polyVoxes[x][y][z][c];
    };


    brush.setPolyVox = function (x, y, z, c, polyVoxID) {
        if (!this.polyVoxes[x]) {
            this.polyVoxes[x] = {};
        }
        if (!this.polyVoxes[x][y]) {
            this.polyVoxes[x][y] = {};
        }
        if (!this.polyVoxes[x][y][z]) {
            this.polyVoxes[x][y][z] = {};
        }
        this.polyVoxes[x][y][z][c] = polyVoxID;
    };


    brush.linkToNeighbors = function (x, y, z, c) {
        // link all the polyVoxes to their neighbors
        var polyVoxID = this.getPolyVox(x, y, z, c);
        if (polyVoxID) {
            var neighborProperties = {};
            var xNNeighborID = this.getPolyVox(x - 1, y, z, c);
            if (xNNeighborID) {
                neighborProperties.xNNeighborID = xNNeighborID;
            }
            var xPNeighborID = this.getPolyVox(x + 1, y, z, c);
            if (xPNeighborID) {
                neighborProperties.xPNeighborID = xPNeighborID;
            }
            var yNNeighborID = this.getPolyVox(x, y - 1, z, c);
            if (yNNeighborID) {
                neighborProperties.yNNeighborID = yNNeighborID;
            }
            var yPNeighborID = this.getPolyVox(x, y + 1, z, c);
            if (yPNeighborID) {
                neighborProperties.yPNeighborID = yPNeighborID;
            }
            var zNNeighborID = this.getPolyVox(x, y, z - 1, c);
            if (zNNeighborID) {
                neighborProperties.zNNeighborID = zNNeighborID;
            }
            var zPNeighborID = this.getPolyVox(x, y, z + 1, c);
            if (zPNeighborID) {
                neighborProperties.zPNeighborID = zPNeighborID;
            }
            Entities.editEntity(polyVoxID, neighborProperties);
        }
    };


    brush.addPolyVoxIfNeeded = function (brushPosition, editSphereRadius) {
        this.polyVoxes = {};
        var halfSliceSize = Vec3.multiply(this.sliceSize, 0.5);
        var sliceRadius = Math.sqrt(Vec3.dot(halfSliceSize, halfSliceSize));
        var capsuleLength = 2 * editSphereRadius + Vec3.length(Vec3.subtract(brushPosition, this.previousBrushPosition));
        var findCenter = Vec3.multiply(Vec3.sum(brushPosition, this.previousBrushPosition), 0.5);
        var findRadius = capsuleLength + sliceRadius + 0.05;

        var ids = Entities.findEntities(findCenter, findRadius * 2);

        var now = Date.now();

        // find all the current polyvox entities
        var withThisColorIDs = [];
        var aetherID = null;
        for (var i = 0; i < ids.length; i++) {
            var possiblePolyVoxID = ids[i];

            var polyVoxProps = this.getPolyVoxProperties(possiblePolyVoxID);
            if (!polyVoxProps) {
                continue;
            }

            if (polyVoxProps.expires - now < 2) {
                // this one is too close to expiring, avoid a race
                Entities.deleteEntity(possiblePolyVoxID);
                continue;
            }

            if (!aetherID) {
                aetherID = polyVoxProps.parentID;
            }

            var userData = JSON.parse(polyVoxProps.userData);
            var cFind = userData.color;

            if (cFind != this.color) {
                continue;
            }

            if (polyVoxProps.parentID != aetherID) {
                continue;
            }

            var polyVoxIndex = userData.index;
            this.setPolyVox(polyVoxIndex.x, polyVoxIndex.y, polyVoxIndex.z, cFind, possiblePolyVoxID);
            withThisColorIDs.push(possiblePolyVoxID);
        }

        var aetherProps;
        if (!aetherID) {
            aetherID = this.addNewAether(brushPosition);
            aetherProps = { id: aetherID, position: brushPosition, rotation: { x: 0, y: 0, z: 0, w: 1 } };
        } else {
            aetherProps = Entities.getEntityProperties(aetherID, ["position", "rotation"]);
            aetherProps.id = aetherID;
        }

        var lowWorldX = this.previousBrushPosition.x - editSphereRadius;
        var lowWorldY = this.previousBrushPosition.y - editSphereRadius;
        var lowWorldZ = this.previousBrushPosition.z - editSphereRadius;
        lowWorldX = Math.min(lowWorldX, brushPosition.x - editSphereRadius);
        lowWorldY = Math.min(lowWorldY, brushPosition.y - editSphereRadius);
        lowWorldZ = Math.min(lowWorldZ, brushPosition.z - editSphereRadius);

        var highWorldX = this.previousBrushPosition.x + editSphereRadius;
        var highWorldY = this.previousBrushPosition.y + editSphereRadius;
        var highWorldZ = this.previousBrushPosition.z + editSphereRadius;
        highWorldX = Math.max(highWorldX, brushPosition.x + editSphereRadius);
        highWorldY = Math.max(highWorldY, brushPosition.y + editSphereRadius);
        highWorldZ = Math.max(highWorldZ, brushPosition.z + editSphereRadius);

        var lowWorld = { x: lowWorldX, y: lowWorldY, z: lowWorldZ };
        var highWorld = { x: highWorldX, y: highWorldY, z: highWorldZ };

        var lowIndex = this.worldPositionToPolyVoxIndex(lowWorld, aetherProps);
        var highIndex = this.worldPositionToPolyVoxIndex(highWorld, aetherProps);

        this.dirtyNeighbors = {};

        for (var x = lowIndex.x; x <= highIndex.x; x++) {
            for (var y = lowIndex.y; y <= highIndex.y; y++) {
                for (var z = lowIndex.z; z <= highIndex.z; z++) {
                    var newID = this.addPolyVox(x, y, z, aetherProps, this.color);
                    if (newID) {
                        this.dirtyNeighbors["" + x + "," + y + "," + z] = true;
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


    brush.addPolyVox = function (x, y, z, aetherProps, c) {
        if (this.getPolyVox(x, y, z, c)) {
            // we already have a polyvox for this position and color
            return null;
        }

        var polyVoxLocalPosition = this.polyVoxIndextoLocalPosition({ x: x, y: y, z: z }, aetherProps);

        var xyzTextures = typeof PALETTE_COLORS[c].textures === 'string' ? [
            PALETTE_COLORS[c].textures,
            PALETTE_COLORS[c].textures,
            PALETTE_COLORS[c].textures
        ] : PALETTE_COLORS[c].textures;

        // offset each color slightly to try to avoid z-buffer fighting
        var colorOffset = Vec3.multiply({ x: c, y: c, z: c }, 0.002);

        var newPolyVoxID = Entities.addEntity({
            type: "PolyVox",
            name: "voxel paint",
            localPosition: Vec3.sum(polyVoxLocalPosition, colorOffset),
            parentID: aetherProps.id,
            dimensions: this.polyVoxSize,
            voxelVolumeSize: { x: this.voxelVolumeSize, y: this.voxelVolumeSize, z: this.voxelVolumeSize },
            voxelSurfaceStyle: 0,
            collisionless: true,
            xTextureURL: xyzTextures[0],
            yTextureURL: xyzTextures[1],
            zTextureURL: xyzTextures[2],
            userData: JSON.stringify({
                color: c,
                grabbableKey: {grabbable: true},
                index: { x: x, y: y, z: z }
            }),
            lifetime: 10
        });

        this.setPolyVox(x, y, z, c, newPolyVoxID);

        if (this.showPolyVoxes) {
            Entities.addEntity({
                name: "voxel paint debug cube",
                type: "Model",
                modelURL: "http://headache.hungry.com/~seth/hifi/voxel-paint-7/unitBoxTransparent.fbx",
                localPosition: { x: 0, y: 0, z: 0 },
                dimensions: this.polyVoxSize,
                collisionless: true,
                parentID: newPolyVoxID
            });
        }

        return newPolyVoxID;
    };


    brush.cleanup = function () {
        if (this.menuMapping) {
            this.menuMapping.disable();
        }
        this.menuMapping = null;
        this.hideMenu();
    };


    return brush;
});
