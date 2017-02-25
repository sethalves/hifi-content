(function() {
    Script.include(Script.resolvePath('voxel-paint-shared.js?v13'));

    var _this = null;

    function HandyAttacher() {
        _this = this;
    }

    HandyAttacher.prototype = {
        attachEntity: function(entityID, attachHand) {
            var attachmentEntity = Entities.addEntity({
                dimensions: {
                    x: 0.62574273347854614,
                    y: 0.62574273347854614,
                    z: 0.62574273347854614
                },
                dynamic: 0,
                name: 'voxel paint palette',
                rotation: {
                    w: 0.89465177059173584,
                    x: 0.022446036338806152,
                    y: 0.43398189544677734,
                    z: -0.10347139835357666
                },
                script: Script.resolvePath('voxel-paint-palette.js') + '?t=' + Date.now(),
                shapeType: 'none',
                collisionless: true,
                type: 'Sphere',
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: true,
                        ignoreIK: false
                    },
                    equipHotspots: [{
                        position: {
                            x: 0.20037400722503662,
                            y: 0.1712799370288849,
                            z: 0.17256569862365723
                        },
                        radius: 0.25,
                        joints: {
                            RightHand: [
                                {
                                    x: 0.06535067409276962,
                                    y: 0.08814819157123566,
                                    z: 0.19130933284759521
                                },
                                {
                                    x: 0.47678816318511963,
                                    y: 0.46527519822120667,
                                    z: -0.5204160213470459,
                                    w: 0.5342028141021729
                                }
                            ],
                            LeftHand: [
                                {
                                    x: -0.03563585877418518,
                                    y: 0.11518450081348419,
                                    z: 0.19681024551391602
                                },
                                {
                                    x: 0.3940891623497009,
                                    y: -0.3781183063983917,
                                    z: 0.4759393334388733,
                                    w: 0.6893547773361206
                                }
                            ]
                        },
                        modelURL: 'http://hifi-content.s3.amazonaws.com/alan/dev/equip-Fresnel-3.fbx',
                        modelScale: {
                            x: 1,
                            y: 1,
                            z: 1
                        }
                    }
                ]}),
                visible: false
            });
            Entities.addEntity({
                dimensions: {
                    x: 0.62574279308319092,
                    y: 0.023471139371395111,
                    z: 0.52269172668457031
                },
                dynamic: 0,
                collisionless: true,
                parentID: attachmentEntity,
                modelURL: attachHand === 'left' ? PALETTE_MODEL_LEFT_HAND : PALETTE_MODEL_RIGHT_HAND,
                name: 'voxel paint palette model',
                rotation: {
                    w: 0.89465177059173584,
                    x: 0.022446036338806152,
                    y: 0.43398189544677734,
                    z: -0.10347139835357666
                },
                shapeType: 'none',
                type: 'Model'
            });
            Script.setTimeout(function() {
                Messages.sendLocalMessage('Hifi-Hand-Grab', JSON.stringify({hand: attachHand, entityID: attachmentEntity}));
            }, 1000);
        },
        startNearTrigger: function(entityID, args) {
            print(' on startNearTrigger!!');
            _this.attachEntity(entityID, args[0]);
        },
        clickReleaseOnEntity: function(entityID, mouseEvent) {
            if (Settings.getValue("io.highfidelity.isEditting")) {
                return;
            }
            _this.attachEntity(entityID, mouseEvent.isLeftButton ? 'left' : 'right');
        }
    };

    return new HandyAttacher();
});
