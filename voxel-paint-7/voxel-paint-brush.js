
/* global Entities, Overlays, Script, Controller, Vec3, Quat, PALETTE_COLORS */
/* jshint loopfunc:true */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    Script.include(Script.resolvePath('voxel-paint-shared.js'));

    var brush = genericTool.genericTool(
        function() { // start
            this.brushHeadID = Entities.getChildrenIDs(this.entityID)[0];
            this.previousBrushPositionSet = false;
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brushHeadID, ["position", "dimensions", "userData"]);
            var editSphereRadius = brushProps.dimensions.x / 2.0;

            if (!this.previousBrushPositionSet) {
                this.previousBrushPositionSet = true;
                this.previousBrushPosition = brushProps.position;
            }

            if (this.eraserEnabled) {
                var searchRadius = 2.0;
                var eraseIDs = Entities.findEntities(brushProps.position, searchRadius);
                for (var i = 0; i < eraseIDs.length; i++) {
                    Entities.setVoxelSphere(eraseIDs[i], brushProps.position, editSphereRadius, 0);
                }
            } else {
                this.color = JSON.parse(brushProps.userData).color;
                var drawOnIDs = this.addPolyVoxIfNeeded(brushProps.position, editSphereRadius);
                for (var j = 0; j < drawOnIDs.length; j++) {
                    Entities.setVoxelCapsule(drawOnIDs[j], this.previousBrushPosition, brushProps.position,
                                             editSphereRadius, 255);
                }
            }

            this.previousBrushPosition = brushProps.position;
        },
        null); // stop

    brush.voxelVolumeSize = 16;
    brush.sliceSize = {x: 0.02, y: 0.02, z: 0.02}; // dimensions of one voxel
    brush.polyVoxSize = Vec3.multiply(brush.sliceSize, brush.voxelVolumeSize); // dimensions of one polyvox entity
    brush.showPolyVoxes = false;
    brush.previousBrushPosition = null;
    brush.previousBrushPositionSet = false;
    brush.polyVoxes = {};
    brush.menuMapping = null;
    brush.menuOverlayIDs = [];
    brush.menuItemWorldPosition = [];
    brush.equipContinued = null;
    brush.eraserEnabled = false;
    brush.activatedMenuItem = null;
    brush.menuItemActivationFuncs = [];

    brush.polyVoxLifetime = 200;
    // brush.polyVoxLifetime = 28800; // 8 hours


    brush.showMenu = function () {
        this.menuOpen = true;
        // var brushProps = Entities.getEntityProperties(this.brushHeadID, ["position", "dimensions"]);
        var brushProps = Entities.getEntityProperties(this.brushHeadID, ["position", "dimensions", "userData"]);
        var handleProps = Entities.getEntityProperties(this.entityID, ["position"]);

        var menuItemCount = PALETTE_COLORS.length + 3; // +3 for erase, bigger, smaller

        // place menu-items around the circle
        var menuItemAngleStep = 360.0 / menuItemCount;
        var up = { x: 0, y: 1, z: 0 };
        var brushVec = Vec3.normalize(Vec3.subtract(brushProps.position, handleProps.position));
        var menuRadius = brushProps.dimensions.x / 2 + 0.2;
        var initialMenuItemVec = Vec3.multiply(Vec3.normalize(Vec3.cross(up, brushVec)), menuRadius);
        var menuRotationVec = Vec3.normalize(Vec3.cross(initialMenuItemVec, up));
        var menuCircumference = 2 * Math.PI * menuRadius;

        this.menuItemSize = (menuCircumference / menuItemCount) - 0.01;
        this.menuActivateRadius = this.menuItemSize / 2;

        var menuItemCreationFuncs = [];
        this.menuItemActivationFuncs = [];
        var menuItemIndex = 0;

        // color change menu items
        for (; menuItemIndex < PALETTE_COLORS.length; menuItemIndex++) {
            menuItemCreationFuncs.push(function (menuItemIndex) {
                var colorOverlayID = Overlays.addOverlay("sphere", {
                    position: menuItemWorldPos,
                    alpha: 0.9,
                    dimensions: brush.menuItemSize,
                    solid: true,
                    color: PALETTE_COLORS[menuItemIndex].color,
                    ignoreRayIntersection: true
                });
                brush.menuOverlayIDs.push(colorOverlayID);
            });
            this.menuItemActivationFuncs.push(function (menuItemIndex, brushProps) {
                var brushUserData = JSON.parse(brushProps.userData);
                brushUserData.color = menuItemIndex;
                Entities.editEntity(brush.brushHeadID, {
                    userData: JSON.stringify(brushUserData),
                    color: PALETTE_COLORS[menuItemIndex].color,
                    alpha: 1.0
                });
                brush.eraserEnabled = false;
            });
        }

        var baseTextOverlayProps = {
            alpha: 0.9,
            dimensions: { x: brush.menuItemSize, y: brush.menuItemSize },
            lineHeight: brush.menuItemSize / 2,
            leftMargin: 0,
            topMargin: 0,
            rightMargin: 0,
            bottomMargin: 0,
            isFacingAvatar: true,
            ignoreRayIntersection: true
        };

        // erase menu item
        menuItemCreationFuncs.push(function () {
            var eraseOverlayProps = baseTextOverlayProps;
            eraseOverlayProps.text = "del";
            eraseOverlayProps.position = menuItemWorldPos;
            var eraseOverlayID = Overlays.addOverlay("text3d", eraseOverlayProps);
            brush.menuOverlayIDs.push(eraseOverlayID);
        });
        this.menuItemActivationFuncs.push(function (menuItemIndex, brushProps) {
            brush.eraserEnabled = true;
            Entities.editEntity(brush.brushHeadID, {
                color: { red: 0, green: 0, blue: 0 },
                alpha: 0.4
            });
        });
        menuItemIndex++;

        // bigger
        menuItemCreationFuncs.push(function () {
            var biggerOverlayProps = baseTextOverlayProps;
            biggerOverlayProps.text = "big";
            biggerOverlayProps.position = menuItemWorldPos;
            var biggerOverlayID = Overlays.addOverlay("text3d", biggerOverlayProps);
            brush.menuOverlayIDs.push(biggerOverlayID);
        });
        this.menuItemActivationFuncs.push(function (menuItemIndex, brushProps) {
            Entities.editEntity(brush.brushHeadID, { dimensions: { x: brushProps.dimensions.x * 1.2,
                                                                   y: brushProps.dimensions.y * 1.2,
                                                                   z: brushProps.dimensions.z * 1.2 } });
        });
        menuItemIndex++;

        // smaller
        menuItemCreationFuncs.push(function () {
            var smallerOverlayProps = baseTextOverlayProps;
            smallerOverlayProps.text = "small";
            smallerOverlayProps.position = menuItemWorldPos;
            var smallerOverlayID = Overlays.addOverlay("text3d", smallerOverlayProps);
            brush.menuOverlayIDs.push(smallerOverlayID);
        });
        this.menuItemActivationFuncs.push(function (menuItemIndex, brushProps) {
            Entities.editEntity(brush.brushHeadID, { dimensions: { x: brushProps.dimensions.x * 0.8,
                                                                   y: brushProps.dimensions.y * 0.8,
                                                                   z: brushProps.dimensions.z * 0.8 } });
        });
        menuItemIndex++;


        for (menuItemIndex = 0; menuItemIndex < menuItemCount; menuItemIndex++) {
            var menuItemAngle = menuItemIndex * menuItemAngleStep;
            var menuItemQuat = Quat.angleAxis(menuItemAngle, menuRotationVec);
            var rotatedMenuItemVec = Vec3.multiplyQbyV(menuItemQuat, initialMenuItemVec);
            var menuItemWorldPos = Vec3.sum(brushProps.position, rotatedMenuItemVec);
            this.menuItemWorldPosition.push(menuItemWorldPos);

            menuItemCreationFuncs[menuItemIndex](menuItemIndex);
        }


        this.equipContinued = function (id, params) {
            var brushProps = Entities.getEntityProperties(brush.brushHeadID, ["position", "userData", "dimensions"]);
            for (var menuItemIndex = 0; menuItemIndex < brush.menuItemWorldPosition.length; menuItemIndex++) {
                var distanceFromMenuItem = Vec3.distance(brushProps.position, brush.menuItemWorldPosition[menuItemIndex]);
                if (distanceFromMenuItem < this.menuActivateRadius) {
                    if (brush.activatedMenuItem == menuItemIndex) {
                        continue;
                    } else {
                        // brush.activateMenuItem(menuItemIndex, brushProps);
                        this.menuItemActivationFuncs[menuItemIndex](menuItemIndex, brushProps);
                        brush.activatedMenuItem = menuItemIndex;
                    }
                } else {
                    if (brush.activatedMenuItem == menuItemIndex) {
                        brush.activatedMenuItem = null;
                    }
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
            this.menuMapping.from(Controller.Standard.LeftPrimaryThumb).peek().to(this.showOrHideMenu);
        } else {
            // equipped in right
            this.menuMapping.from(Controller.Standard.RightPrimaryThumb).peek().to(this.showOrHideMenu);
        }
        Controller.enableMapping(mappingName);
    };


    brush.equipStopped = function () {
        this.menuMapping.disable();
        this.menuMapping = null;
        this.hideMenu();
    };


    brush.polyVoxCache = {};

    brush.getPolyVoxProperties = function (polyVoxID) {
        var polyVoxProps = this.polyVoxCache[polyVoxID];
        if (!polyVoxProps) {
            polyVoxProps = Entities.getEntityProperties(polyVoxID, ["name", "position", "userData",
                                                                    "age", "lifetime", "parentID",
                                                                    "xNNeighborID", "xPNeighborID", "yNNeighborID",
                                                                    "yPNeighborID", "zNNeighborID", "zPNeighborID"]);
            if (polyVoxProps.name != "voxel paint") {
                return null;
            }
            this.polyVoxCache[polyVoxID] = polyVoxProps;
        }

        var now = Date.now();
        polyVoxProps.expires = now + polyVoxProps.lifetime - polyVoxProps.age;
        if (polyVoxProps.expires - now < 2) {
            // try to avoid races
            delete this.polyVoxProps[polyVoxID];
            Entities.deleteEntity(polyVoxID);
            return null;
        }

        return polyVoxProps;
    };


    brush.editPolyVox = function (polyVoxID, editProps) {
        var polyVoxProps = this.polyVoxCache[polyVoxID];
        if (polyVoxProps) {
            for (var editKey in editProps) {
                if (editProps.hasOwnProperty(editKey)) {
                    polyVoxProps[editKey] = editProps[editKey];
                }
            }
            this.polyVoxCache[polyVoxID] = polyVoxProps;
        }
        Entities.editEntity(polyVoxID, editProps);
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


    brush.getPolyVox = function (aetherID, x, y, z, c) {
        if (!this.polyVoxes[aetherID]) {
            return null;
        }
        if (!this.polyVoxes[aetherID][x]) {
            return null;
        }
        if (!this.polyVoxes[aetherID][x][y]) {
            return null;
        }
        if (!this.polyVoxes[aetherID][x][y][z]) {
            return null;
        }
        return this.polyVoxes[aetherID][x][y][z][c];
    };


    brush.setPolyVox = function (aetherID, x, y, z, c, polyVoxID) {
        if (!this.polyVoxes[aetherID]) {
            this.polyVoxes[aetherID] = {};
        }
        if (!this.polyVoxes[aetherID][x]) {
            this.polyVoxes[aetherID][x] = {};
        }
        if (!this.polyVoxes[aetherID][x][y]) {
            this.polyVoxes[aetherID][x][y] = {};
        }
        if (!this.polyVoxes[aetherID][x][y][z]) {
            this.polyVoxes[aetherID][x][y][z] = {};
        }
        this.polyVoxes[aetherID][x][y][z][c] = polyVoxID;
    };


    brush.linkToNeighbors = function (aetherID, x, y, z, c) {
        // link all the polyVoxes to their neighbors
        var polyVoxID = this.getPolyVox(aetherID, x, y, z, c);
        if (polyVoxID) {
            var neighborProperties = {};
            var xNNeighborID = this.getPolyVox(aetherID, x - 1, y, z, c);
            if (xNNeighborID) {
                neighborProperties.xNNeighborID = xNNeighborID;
            }
            var xPNeighborID = this.getPolyVox(aetherID, x + 1, y, z, c);
            if (xPNeighborID) {
                neighborProperties.xPNeighborID = xPNeighborID;
            }
            var yNNeighborID = this.getPolyVox(aetherID, x, y - 1, z, c);
            if (yNNeighborID) {
                neighborProperties.yNNeighborID = yNNeighborID;
            }
            var yPNeighborID = this.getPolyVox(aetherID, x, y + 1, z, c);
            if (yPNeighborID) {
                neighborProperties.yPNeighborID = yPNeighborID;
            }
            var zNNeighborID = this.getPolyVox(aetherID, x, y, z - 1, c);
            if (zNNeighborID) {
                neighborProperties.zNNeighborID = zNNeighborID;
            }
            var zPNeighborID = this.getPolyVox(aetherID, x, y, z + 1, c);
            if (zPNeighborID) {
                neighborProperties.zPNeighborID = zPNeighborID;
            }
            this.editPolyVox(polyVoxID, neighborProperties);
        }
    };


    brush.addPolyVoxIfNeeded = function (brushPosition, editSphereRadius) {
        this.polyVoxes = {};
        var halfSliceSize = Vec3.multiply(this.sliceSize, 0.5);
        var sliceRadius = Math.sqrt(Vec3.dot(halfSliceSize, halfSliceSize));
        var capsuleLength = 2 * editSphereRadius + Vec3.length(Vec3.subtract(brushPosition, this.previousBrushPosition));
        var findCenter = Vec3.multiply(Vec3.sum(brushPosition, this.previousBrushPosition), 0.5);
        var findRadius = capsuleLength + sliceRadius + 0.05;

        var ids = Entities.findEntities(findCenter, findRadius);

        // find all the current polyvox entities
        var withThisColorIDs = [];
        var aetherID = null;
        for (var i = 0; i < ids.length; i++) {
            var possiblePolyVoxID = ids[i];

            var polyVoxProps = this.getPolyVoxProperties(possiblePolyVoxID);
            if (!polyVoxProps) {
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
            this.setPolyVox(aetherID, polyVoxIndex.x, polyVoxIndex.y, polyVoxIndex.z, cFind, possiblePolyVoxID);
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
                this.linkToNeighbors(aetherID, nX, nY, nZ, this.color);
            }
        }

        return withThisColorIDs;
    };


    brush.addPolyVox = function (x, y, z, aetherProps, c) {
        if (this.getPolyVox(aetherProps.id, x, y, z, c)) {
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

        var props = {
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
            // lifetime: 10
            lifetime: this.polyVoxLifetime
        };
        var newPolyVoxID = Entities.addEntity(props);
        this.polyVoxCache[newPolyVoxID] = props;

        this.setPolyVox(aetherProps.id, x, y, z, c, newPolyVoxID);

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
