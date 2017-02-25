(function() {
    Script.include(Script.resolvePath('voxel-paint-shared.js?v13'));

    function newColor(paletteEntity, color, colorIndex, hand) {
        var newOverlayProperties = {
            parentID: paletteEntity,
            localPosition: {
                x: hand === 'left' ? -color.position.x : color.position.x,
                y: color.position.y,
                z: color.position.z
            },
            localRotation: {x: 0.0, y: 0.0, z: 0.0, w: 1.0},
            alpha: 0.0,
            //visible: false,
            solid: true,
            dimensions: color.dimensions,
            color: color.color
        };
        return {
            colorIndex: colorIndex,
            overlay: Overlays.addOverlay('cube', newOverlayProperties),
            overlayProperties: newOverlayProperties
        };
    }

    function newTool(paletteEntity, tool, toolIndex, hand) {
        var newOverlayProperties = {
            parentID: paletteEntity,
            localPosition: {
                x: hand === 'left' ? -tool.paletteProperties.position.x : tool.paletteProperties.position.x,
                y: tool.paletteProperties.position.y,
                z: tool.paletteProperties.position.z
            },
            localRotation: hand === 'left' ?
                tool.paletteProperties.leftRotation :
                tool.paletteProperties.rightRotation,
            dimensions: tool.paletteProperties.dimensions,
            url: tool.properties.modelURL
        };

        return {
            toolIndex: toolIndex,
            overlay: Overlays.addOverlay('model', newOverlayProperties),
            overlayProperties: newOverlayProperties
        };
    }

    function switchLeftRight(side) {
        return side === 'left' ? 'right' : 'left';
    }

    var _this;

    function Palette() {
        _this = this;
        this.equipped = false;
    }

    Palette.prototype = {
        preload: function(id) {
            print('preloaded ' + id);
            _this.colors = [];
            _this.tools = [];
            _this.entityID = id;
            _this.toolEntity = null;
            _this.voxelPaintToolEntity = null;
        },
        colorFromOverlay: function(overlayID) {
            for (var i = 0; i < _this.colors.length; i++) {
                if (_this.colors[i].overlay === overlayID) {
                    return _this.colors[i];
                }
            }
            return null;
        },
        toolFromOverlay: function(overlayID) {
            for (var i = 0; i < _this.tools.length; i++) {
                if (_this.tools[i].overlay === overlayID) {
                    return _this.tools[i];
                }
            }
            return null;
        },
        startEquip: function(id, params) {
            print('Start equip here.');
            _this.equipped = true;
            _this.startEquipTime = Date.now();

            _this.paletteModelEntity = Entities.getChildrenIDs(id)[0];

            var paletteModelProperties = Entities.getEntityProperties(_this.paletteModelEntity);
            _this.paletteModelOverlay = Overlays.addOverlay('cube', {
                parentID: paletteModelProperties.parentID,
                localPosition: paletteModelProperties.localPosition,
                localRotation: paletteModelProperties.localRotation,
                alpha: 0.0,
                solid: true,
                dimensions: paletteModelProperties.dimensions
            });

            PALETTE_COLORS.forEach(function(color, colorIndex) {
                _this.colors.push(newColor(_this.entityID, color, colorIndex, params[0]));
            });

            VOXEL_TOOLS.forEach(function(tool, toolIndex) {
                _this.tools.push(newTool(_this.entityID, tool, toolIndex, params[0]));
            });

            Entities.editEntity(_this.paletteModelEntity, {
                modelURL: params[0] === 'left' ? PALETTE_MODEL_LEFT_HAND : PALETTE_MODEL_RIGHT_HAND
            });

            if (_this.voxelPaintToolEntity !== null) {
                Entities.deleteEntity(_this.voxelPaintToolEntity);
            }

            // for now lets equip a tool in the right hand whenever the palette is equipped
            _this.voxelPaintToolEntity = Entities.addEntity({
                name: 'voxel paint tool',
                dimensions: {
                    x: 0.60412204265594482,
                    y: 0.60412204265594482,
                    z: 0.60412204265594482
                },
                rotation: {
                    w: 0.7071068286895752,
                    x: 0,
                    y: 0,
                    z: -0.7071068286895752
                },
                script: Script.resolvePath('voxel-paint-tool.js?t=' + Date.now()),
                type: 'Sphere',
                collisionless: true,
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: true,
                        ignoreIK: false
                    },
                    equipHotspots: [{
                        position: {x: 0.11031082272529602, y: 0.19449540972709656, z: 0.0405043363571167},
                        radius: 0.25,
                        joints: {
                            RightHand: [
                                {x: 0.11031082272529602, y: 0.19449540972709656, z: 0.0405043363571167},
                                {x: 0.2807741165161133, y: 0.6332069635391235, z: 0.2997693121433258, w: -0.6557632088661194}
                            ],
                            LeftHand: [
                                {x: -0.10801754891872406, y: 0.15447449684143066, z: 0.030637264251708984},
                                {x: -0.32700979709625244, y: 0.623619794845581, z: 0.28943854570388794, w: 0.6483823657035828}
                            ]
                        },
                        modelURL: 'http://hifi-content.s3.amazonaws.com/alan/dev/equip-Fresnel-3.fbx',
                        modelScale: {
                            x: 1,
                            y: 1,
                            z: 1
                        }
                    }]
                }),
                visible: false
            });
            Entities.addEntity({
                name: 'voxel paint tool',
                parentID: _this.voxelPaintToolEntity,
                dimensions: {
                    x: 0.012468120083212852,
                    y: 0.60412204265594482,
                    z: 0.012788690626621246
                },
                collisionless: true,
                modelURL: MODELS_PATH + 'smallBrush.fbx',
                localPosition: {x: 0, y: 0, z: 0},
                shapeType: 'none',
                type: 'Model'
            });
            Script.setTimeout(function() {
                Messages.sendLocalMessage('Hifi-Hand-Grab', JSON.stringify({
                    hand: switchLeftRight(params[0]),
                    entityID: _this.voxelPaintToolEntity
                }));

            }, 1000);
        },
        continueEquip: function(id, params) {

        },
        cleanup: function() {
            _this.colors.forEach(function(color) {
                Overlays.deleteOverlay(color.overlay);
            });
            _this.colors = [];
            _this.tools.forEach(function(tool) {
                Overlays.deleteOverlay(tool.overlay);
            });
            _this.tools = [];
            Overlays.deleteOverlay(_this.paletteModelOverlay);
            _this.paletteModelOverlay = null;
        },
        pointingAtOverlay: function(entityID, args) {
            var voxelPaintTool = JSON.parse(args[0]);

            var colorOverlayResult = _this.colorFromOverlay(voxelPaintTool.selectedOverlayID);
            if (colorOverlayResult !== null) {
                if (colorOverlayResult.colorIndex === voxelPaintTool.currentColor) {
                    return;
                }
                Entities.callEntityMethod(voxelPaintTool.voxelPaintToolID, 'setColor', [colorOverlayResult.colorIndex]);
                return;
            }

            var toolOverlayResult = _this.toolFromOverlay(voxelPaintTool.selectedOverlayID);
            if (toolOverlayResult !== null) {
                if (toolOverlayResult.toolIndex === voxelPaintTool.currentTool) {
                    return;
                }
                Entities.callEntityMethod(voxelPaintTool.voxelPaintToolID, 'setTool', [toolOverlayResult.toolIndex]);
                return;
            }
        },
        releaseEquip: function(id, params) {
            _this.cleanup();
            // FIXME: it seems to release quickly while auto-attaching, added this to make sure it doesn't delete the entity at that time
            var MIN_CLEANUP_TIME = 1000;
            if (_this.equipped && (Date.now() - _this.startEquipTime) > 1000) {
                Entities.deleteEntity(_this.entityID);
            }
            _this.equipped = false;
        },
        unload: function() {
            _this.cleanup();
        }
    };

    return new Palette();
});
