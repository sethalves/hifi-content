
/* global Entities, genericTool, Script, Vec3, Quat, textureIndexToURLs, paintBucketColors */

(function() {
    Script.include(Script.resolvePath('voxel-paint-shared.js?v13'));
    var _this;

    var NULL_UUID = '{00000000-0000-0000-0000-000000000000}';

    var TRIGGER_CONTROLS = [
        Controller.Standard.LT,
        Controller.Standard.RT
    ];
    var RELOAD_THRESHOLD = 0.3;

    var SELECTION_SPHERE_DIMENSIONS = {x: 0.05, y: 0.05, z: 0.05};

    VoxelPaintTool = function() {
        _this = this;
        _this.selectionSphere = false;
        _this.slices = 20;
        _this.voxelSize = 16;
        _this.showPolyVoxes = false;
        _this.toolRadius = 0.025;
        _this.toolLength = 0.275;
        _this.color = 0;
        _this.tool = 0;
        _this.toolEntity = null;
    };

    VoxelPaintTool.prototype = {
        preload: function(id) {
            _this.active = false;
            _this.entityID = id;
            _this.brushOverlay = null;
        },
        // remote called function
        setColor: function(id, params) {
            _this.color = params[0];
            if (VOXEL_TOOLS[_this.tool].type === 'brush') {
                Overlays.editOverlay(_this.brushOverlay, {
                    color: PALETTE_COLORS[_this.color].color
                });
            }
        },
        setTool: function(id, params) {
            _this.tool = params[0];
            var voxelTool = VOXEL_TOOLS[_this.tool];
            Entities.editEntity(_this.toolEntity, voxelTool.properties);
            _this.toolRadius = voxelTool.toolRadius;
            _this.toolLength = voxelTool.toolLength;
            if (voxelTool.type === 'brush') {
                Overlays.editOverlay(_this.brushOverlay, {
                    color: PALETTE_COLORS[_this.color].color
                });
            } else if (voxelTool.type === 'eraser') {
                Overlays.editOverlay(_this.brushOverlay, {
                    color: {red: 0, blue: 0, green: 0}
                });
            }
            Overlays.editOverlay(_this.brushOverlay, {
                dimensions: _this.showSelectionSphere ? SELECTION_SPHERE_DIMENSIONS : {x: _this.toolRadius * 2, y: _this.toolRadius * 2, z: _this.toolRadius * 2},
                localPosition: {x: 0, y: _this.toolLength, z: 0}
            });
        },
        startEquip: function(id, params) {
            _this.toolEntity = Entities.getChildrenIDs(id)[0];
            _this.equipped = true;
            _this.startEquipTime = Date.now();
            _this.hand = params[0] == 'left' ? 0 : 1;
            _this.brushOverlay = Overlays.addOverlay('sphere', {
                parentID: id,
                dimensions: {x: _this.toolRadius * 2, y: _this.toolRadius * 2, z: _this.toolRadius * 2},
                localPosition: {x: 0, y: _this.toolLength, z: 0},
                color: PALETTE_COLORS[_this.color].color,
                solid: true,
                alpha: 1.0,
                ignoreRayIntersection: true
            });
        },
        toggleWithTriggerPressure: function() {
            _this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[_this.hand]);

            if (_this.triggerValue < RELOAD_THRESHOLD) {
                _this.targetEntity = null;
                _this.targetAvatar = null;
                _this.canActivate = true;
                _this.active = false;
            }
            if (_this.canActivate === true && _this.triggerValue > 0.55) {
                _this.canActivate = false;
                _this.active = true;
            }
        },
        continueEquip: function(id, params) {
            _this.toggleWithTriggerPressure();
            var properties = Entities.getEntityProperties(id, ['position', 'rotation']);
            if (_this.active) {
                var brushPosition = Vec3.sum(Vec3.multiplyQbyV(properties.rotation, {x: 0, y: _this.toolLength, z: 0}), properties.position);
                var toolType = VOXEL_TOOLS[_this.tool].type;
                if (toolType === 'brush') {
                    var ids = _this.addPolyVoxIfNeeded(brushPosition, _this.toolRadius);
                    for (var i = 0; i < ids.length; i++) {
                        Entities.setVoxelSphere(ids[i], brushPosition, _this.toolRadius, 255);
                    }
                } else if (toolType === 'eraser') {
                    var searchRadius = 2.0;
                    var ids = Entities.findEntities(brushPosition, searchRadius);
                    for (var i = 0; i < ids.length; i++) {
                        Entities.setVoxelSphere(ids[i], brushPosition, _this.toolRadius, 0);
                    }
                }
            } else {
                var beamLength = _this.toolLength + _this.toolRadius;
                var pickRay = {
                    origin: properties.position,
                    direction: Quat.getUp(properties.rotation),
                    length: _this.toolLength + _this.toolRadius
                };
                var result = Overlays.findRayIntersection(pickRay);
                if (result.intersects && result.distance < beamLength) {
                    var parentID = Overlays.getProperty(result.overlayID, 'parentID');
                    if (parentID === null || parentID === NULL_UUID) {
                        return;
                    }
                    Entities.callEntityMethod(parentID, 'pointingAtOverlay', [JSON.stringify({
                        voxelPaintToolID: id,
                        currentTool: _this.tool,
                        currentColor: _this.color,
                        selectedOverlayID: result.overlayID
                    })]);
                    _this.showSelectionSphere(true);
                } else {
                    _this.showSelectionSphere(false);
                }

            }
        },
        showSelectionSphere: function(showSelectionSphere) {
            if (_this.selectionSphere === showSelectionSphere) {
                return;
            }
            _this.selectionSphere = showSelectionSphere;
            if (_this.selectionSphere) {
                Overlays.editOverlay(_this.brushOverlay, {
                    dimensions: SELECTION_SPHERE_DIMENSIONS
                });
                return;
            }
            Overlays.editOverlay(_this.brushOverlay, {
                dimensions: {x: _this.toolRadius * 2, y: _this.toolRadius * 2, z: _this.toolRadius * 2}
            });
        },
        cleanup: function() {
            if (_this.brushOverlay !== null) {
                Overlays.deleteOverlay(_this.brushOverlay);
                _this.brushOverlay = null;
            }
        },
        releaseEquip: function(id, params) {
            _this.cleanup();
            // FIXME: it seems to release quickly while auto-attaching, added this to make sure it doesn't delete the entity at that time
            var MIN_CLEANUP_TIME = 1000;
            if (_this.equipped && (Date.now() - _this.startEquipTime) > 1000) {
                Entities.deleteEntity(_this.entityID);
            }
        },
        unload: function() {
            _this.cleanup();
        },
        getPolyVox: function (x, y, z, c) {
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
        },
        linkToNeighbors: function (x, y, z, c) {
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
        },
        clamp: function (v) {
            return Math.min(Math.max(v, 0), this.slices - 1);
        },
        addPolyVoxIfNeeded: function (brushPosition, editSphereRadius) {
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

            // find the aether
            var aetherID = null;
            for (i = 0; i < ids.length; i++) {
                var possibleAetherID = ids[i];
                if (props[possibleAetherID].name == "voxel paint aether") {
                    aetherID = possibleAetherID;
                    break;
                }
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

                if (cFind != this.color) {
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
                            var newID = this.addPolyVox(x, y, z, this.color, sliceSize, aetherID, aetherProps.dimensions);
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
        },
        addPolyVox: function (x, y, z, c, sliceSize, aetherID, aetherDimensions) {
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

            var xyzTextures = typeof PALETTE_COLORS[c].textures === 'string' ? [
                    PALETTE_COLORS[c].textures,
                    PALETTE_COLORS[c].textures,
                    PALETTE_COLORS[c].textures
                ] : PALETTE_COLORS[c].textures;

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
                xTextureURL: xyzTextures[0],
                yTextureURL: xyzTextures[1],
                zTextureURL: xyzTextures[2],
                userData: JSON.stringify({color: c}),
                parentID: aetherID
            });

            if (_this.showPolyVoxes) {
                Entities.addEntity({
                    name: "voxel paint debug cube",
                    type: "Model",
                    modelURL:  MODELS_PATH + 'unitBoxTransparent.fbx',
                    localPosition: localPosition,
                    dimensions: sliceSize,
                    collisionless: true,
                    // lifetime: 60.0
                    parentID: aetherID
                });
            }

            return this.polyvoxes[x][y][z][c];
        }
    };

    return new VoxelPaintTool();
});
