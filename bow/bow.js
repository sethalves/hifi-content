//
//  bow.js
//
//  This script attaches to a bow that you can pick up with a hand controller.
//  Created by James B. Pollack @imgntn on 10/19/2015
//  Copyright 2015 High Fidelity, Inc.
//
//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html
//

/*global Script, Controller, SoundCache, Entities, getEntityCustomData, setEntityCustomData, MyAvatar, Vec3, Quat, Messages */

(function() {

    Script.include("/~/system/libraries/utils.js");

    var NULL_UUID = "{00000000-0000-0000-0000-000000000000}";

    var NOTCH_ARROW_SOUND_URL = 'http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/notch.wav';
    var SHOOT_ARROW_SOUND_URL =
        'http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/String_release2.L.wav';
    var STRING_PULL_SOUND_URL = 'http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/Bow_draw.1.L.wav';
    var ARROW_HIT_SOUND_URL = 'http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/Arrow_impact1.L.wav';

    var ARROW_OFFSET = -0.44;
    var ARROW_TIP_OFFSET = 0.47;
    var ARROW_GRAVITY = {
        x: 0,
        y: -4.8,
        z: 0
    };

    var ARROW_MODEL_URL = "http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/newarrow_textured.fbx";
    var ARROW_COLLISION_HULL_URL =
        "http://mpassets.highfidelity.com/32fc6d32-27a2-428e-937e-869f3f05e8e1-v1/newarrow_collision_hull.obj";

    var ARROW_DIMENSIONS = {
        x: 0.03,
        y: 0.03,
        z: 0.96
    };

    var ARROW_LIFETIME = 15; // seconds


    var TOP_NOTCH_OFFSET = 0.6;
    var BOTTOM_NOTCH_OFFSET = 0.6;

    var LINE_DIMENSIONS = {
        x: 5,
        y: 5,
        z: 5
    };

    var DRAW_STRING_THRESHOLD = 0.80;
    var DRAW_STRING_PULL_DELTA_HAPTIC_PULSE = 0.09;
    var DRAW_STRING_MAX_DRAW = 0.7;
    var MAX_NEW_ARROW_PULLBACK_DISTANCE = 0.32; // more than this and a new arrow doesn't start

    var NOTCH_OFFSET_FORWARD = 0.08;
    var NOTCH_OFFSET_UP = 0.035;

    var SHOT_SCALE = {
        min1: 0,
        max1: 0.6,
        min2: 1,
        max2: 15
    };

    var USE_DEBOUNCE = false;

    var TRIGGER_CONTROLS = [
        Controller.Standard.LT,
        Controller.Standard.RT,
    ];

    function interval() {
        var lastTime = new Date().getTime();

        return function getInterval() {
            var newTime = new Date().getTime();
            var delta = newTime - lastTime;
            lastTime = newTime;
            return delta;
        };
    }

    var checkInterval = interval();

    var _this;

    function Bow() {
        _this = this;
        return;
    }

    Bow.prototype = {
        stringDrawn: false,
        aiming: false,
        arrowTipPosition: null,
        preNotchString: null,
        hasArrowNotched: false,
        arrow: null,
        stringData: {
            currentColor: {
                red: 255,
                green: 255,
                blue: 255
            }
        },
        sinceLastUpdate: 0,
        preload: function(entityID) {
            this.entityID = entityID;
            this.stringPullSound = SoundCache.getSound(STRING_PULL_SOUND_URL);
            this.shootArrowSound = SoundCache.getSound(SHOOT_ARROW_SOUND_URL);
            this.arrowHitSound = SoundCache.getSound(ARROW_HIT_SOUND_URL);
            this.arrowNotchSound = SoundCache.getSound(NOTCH_ARROW_SOUND_URL);
            var userData = Entities.getEntityProperties(this.entityID, ["userData"]).userData;
            this.userData = JSON.parse(userData);
            var children = Entities.getChildrenIDs(this.entityID);
            children.forEach(function(childID) {
                var childName = Entities.getEntityProperties(childID, ["name"]).name;
                if (childName == "Hifi-Bow-Pre-Notch-String") {
                    this.preNotchString = children[0];
                }
            });
        },

        unload: function() {
            this.deleteStrings();
            Entities.deleteEntity(this.arrow);
        },

        startNearGrab: function(entityID, args) {
            _this.startEquip(entityID, args);
        },

        continueNearGrab: function(entityID, args) {
            _this.continueEquip(entityID, args);
        },

        releaseGrab: function(entityID, args) {
            _this.releaseEquip(entityID, args);
        },

        startEquip: function(entityID, args) {
            this.hand = args[0];
            // var avatarID = args[1];

            //disable the opposite hand in handControllerGrab.js by message
            var handToDisable = this.hand === 'right' ? 'left' : 'right';
            Messages.sendMessage('Hifi-Hand-Disabler', handToDisable);

            var data = getEntityCustomData('grabbableKey', this.entityID, {});
            data.grabbable = false;
            setEntityCustomData('grabbableKey', this.entityID, data);
            Entities.editEntity(_this.entityID, {
                collidesWith: ""
            });

            //make sure the string is ready
            if (!this.preNotchString) {
                this.createPreNotchString();
            }
            var preNotchStringProps = Entities.getEntityProperties(this.preNotchString);
            if (!preNotchStringProps || preNotchStringProps.name != "Hifi-Bow-Pre-Notch-String") {
                this.createPreNotchString();
            }
            Entities.editEntity(this.preNotchString, {
                visible: true
            });
        },
        continueEquip: function(entityID, args) {
            this.deltaTime = checkInterval();
            //debounce during debugging -- maybe we're updating too fast?
            if (USE_DEBOUNCE === true) {
                this.sinceLastUpdate = this.sinceLastUpdate + this.deltaTime;

                if (this.sinceLastUpdate > 60) {
                    this.sinceLastUpdate = 0;
                } else {
                    return;
                }
            }

            this.checkStringHand();
        },
        releaseEquip: function(entityID, args) {
            Messages.sendMessage('Hifi-Hand-Disabler', "none");

            this.stringDrawn = false;
            this.deleteStrings();

            var data = getEntityCustomData('grabbableKey', this.entityID, {});
            data.grabbable = true;
            setEntityCustomData('grabbableKey', this.entityID, data);
            Entities.deleteEntity(this.arrow);
            this.aiming = false;
            this.hasArrowNotched = false;
            Entities.editEntity(_this.entityID, {
                collidesWith: "static,dynamic,kinematic,otherAvatar,myAvatar"
            });
        },

        createArrow: function() {
            this.playArrowNotchSound();

            var arrow = Entities.addEntity({
                name: 'Hifi-Arrow',
                type: 'Model',
                modelURL: ARROW_MODEL_URL,
                shapeType: 'compound',
                compoundShapeURL: ARROW_COLLISION_HULL_URL,
                dimensions: ARROW_DIMENSIONS,
                position: this.bowProperties.position,
                parentID: this.entityID,
                dynamic: false,
                collisionless: true,
                collisionSoundURL: ARROW_HIT_SOUND_URL,
                damping: 0.01,
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    },
                    creatorSessionUUID: MyAvatar.sessionUUID
                })
            });

            var makeArrowStick = function(entityA, entityB, collision) {
                Entities.editEntity(entityA, {
                    localAngularVelocity: {
                        x: 0,
                        y: 0,
                        z: 0
                    },
                    localVelocity: {
                        x: 0,
                        y: 0,
                        z: 0
                    },
                    gravity: {
                        x: 0,
                        y: 0,
                        z: 0
                    },
                    parentID: entityB,
                    dynamic: false,
                    collisionless: true,
                    collidesWith: ""
                });
                Script.removeEventHandler(arrow, "collisionWithEntity", makeArrowStick);
            };

            Script.addEventHandler(arrow, "collisionWithEntity", makeArrowStick);

            return arrow;
        },

        createPreNotchString: function() {
            this.preNotchString = Entities.addEntity({
                "collisionless": 1,
                "dimensions": { "x": 5, "y": 5, "z": 5 },
                "ignoreForCollisions": 1,
                "linePoints": [ { "x": 0, "y": 0, "z": 0 }, { "x": 0, "y": -1.2, "z": 0 } ],
                "lineWidth": 5,
                "name": "Hifi-Bow-Pre-Notch-String",
                "parentID": this.entityID,
                "localPosition": { "x": 0, "y": 0.6, "z": 0.1 },
                "localRotation": { "w": 1, "x": 0, "y": 0, "z": 0 },
                "type": "Line",
                "userData": "{\"grabbableKey\":{\"grabbable\":false}}"
            });
        },

        createStrings: function() {
            this.createTopString();
        },

        createTopString: function() {
            var stringProperties = {
                name: 'Hifi-Bow-Top-String',
                type: 'Line',
                position: Vec3.sum(this.bowProperties.position, TOP_NOTCH_OFFSET),
                dimensions: LINE_DIMENSIONS,
                dynamic: false,
                collisionless: true,
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    }
                })
            };

            this.topString = Entities.addEntity(stringProperties);
        },

        deleteStrings: function() {
            Entities.deleteEntity(this.topString);
        },

        updateStringPositions: function() {
            var upVector = Quat.getUp(this.bowProperties.rotation);
            var upOffset = Vec3.multiply(upVector, TOP_NOTCH_OFFSET);
            var downVector = Vec3.multiply(-1, Quat.getUp(this.bowProperties.rotation));
            var downOffset = Vec3.multiply(downVector, BOTTOM_NOTCH_OFFSET);
            var backOffset = Vec3.multiply(-0.1, Quat.getFront(this.bowProperties.rotation));

            var topStringPosition = Vec3.sum(this.bowProperties.position, upOffset);
            this.topStringPosition = Vec3.sum(topStringPosition, backOffset);
            var bottomStringPosition = Vec3.sum(this.bowProperties.position, downOffset);
            this.bottomStringPosition = Vec3.sum(bottomStringPosition, backOffset);

            Entities.editEntity(this.topString, {
                position: this.topStringPosition
            });
        },

        drawStrings: function() {

            this.updateStringPositions();
            var lineVectors = this.getLocalLineVectors();

            Entities.editEntity(this.topString, {
                linePoints: [{ x: 0, y: 0, z: 0 }, lineVectors[0], lineVectors[1]],
                lineWidth: 5,
                color: this.stringData.currentColor
            });
        },

        getLocalLineVectors: function() {
            var topVector = Vec3.subtract(this.arrowRearPosition, this.topStringPosition);
            var bottomVector = Vec3.subtract(this.bottomStringPosition, this.topStringPosition);
            return [topVector, bottomVector];
        },

        checkStringHand: function() {
            //invert the hands because our string will be held with the opposite hand of the first one we pick up the bow with
            var triggerLookup;
            if (this.hand === 'left') {
                triggerLookup = 1;
                this.getStringHandPosition = MyAvatar.getRightPalmPosition;
            } else if (this.hand === 'right') {
                this.getStringHandPosition = MyAvatar.getLeftPalmPosition;
                triggerLookup = 0;
            }

            this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[triggerLookup]);

            if (this.triggerValue < DRAW_STRING_THRESHOLD && this.stringDrawn === true) {
                // firing the arrow
                this.bowProperties = Entities.getEntityProperties(this.entityID);
                this.drawStrings();
                this.hasArrowNotched = false;
                this.aiming = false;
                this.stringDrawn = false;
                this.updateArrowPositionInNotch(true);
                Entities.editEntity(this.preNotchString, { visible: true });

            } else if (this.triggerValue > DRAW_STRING_THRESHOLD && this.stringDrawn === true) {
                //continuing to aim the arrow
                this.bowProperties = Entities.getEntityProperties(this.entityID);
                this.aiming = true;
                this.drawStrings();
                this.updateArrowPositionInNotch();

            } else if (this.triggerValue > DRAW_STRING_THRESHOLD && this.stringDrawn === false) {
                //the first time aiming the arrow
                this.bowProperties = Entities.getEntityProperties(this.entityID);

                // only start a new arrow if they back hand was close to the string
                var notchPosition = this.getNotchPosition(this.bowProperties);
                var stringHandPosition = this.getStringHandPosition();
                var handToNotch = Vec3.subtract(notchPosition, stringHandPosition);
                var pullBackDistance = Vec3.length(handToNotch);
                if (pullBackDistance < MAX_NEW_ARROW_PULLBACK_DISTANCE) {
                    this.arrow = this.createArrow();
                    this.playStringPullSound();
                    Entities.editEntity(this.preNotchString, { visible: false });
                    this.pullBackDistance = 0;
                    this.stringDrawn = true;
                    this.createStrings();
                    this.drawStrings();
                    this.updateArrowPositionInNotch();
                }

            }
        },

        setArrowRearPosition: function(arrowPosition, arrowRotation) {
            var frontVector = Quat.getFront(arrowRotation);
            var frontOffset = Vec3.multiply(frontVector, -ARROW_TIP_OFFSET);
            var arrorRearPosition = Vec3.sum(arrowPosition, frontOffset);
            this.arrowRearPosition = arrorRearPosition;
            return arrorRearPosition;

        },

        getNotchPosition: function(bowProperties) {
            var frontVector = Quat.getFront(bowProperties.rotation);
            var notchVectorForward = Vec3.multiply(frontVector, NOTCH_OFFSET_FORWARD);
            var upVector = Quat.getUp(bowProperties.rotation);
            var notchVectorUp = Vec3.multiply(upVector, NOTCH_OFFSET_UP);
            var notchPosition = Vec3.sum(bowProperties.position, notchVectorForward);
            notchPosition = Vec3.sum(notchPosition, notchVectorUp);
            return notchPosition;
        },

        updateArrowPositionInNotch: function(shouldReleaseArrow) {
            //set the notch that the arrow should go through
            var notchPosition = this.getNotchPosition(this.bowProperties);
            //set the arrow rotation to be between the notch and other hand
            var stringHandPosition = this.getStringHandPosition();
            var handToNotch = Vec3.subtract(notchPosition, stringHandPosition);
            var arrowRotation = Quat.rotationBetween(Vec3.FRONT, handToNotch);

            var backHand = this.hand === 'left' ? 1 : 0;
            var pullBackDistance = Vec3.length(handToNotch);
            // pulse as arrow is drawn
            if (Math.abs(pullBackDistance - this.pullBackDistance) > DRAW_STRING_PULL_DELTA_HAPTIC_PULSE) {
                // Controller.triggerShortHapticPulse(1, backHand);
                Controller.triggerHapticPulse(1, 20, backHand);
                this.pullBackDistance = pullBackDistance;
            }
            // this.changeStringPullSoundVolume(pullBackDistance);

            if (pullBackDistance > DRAW_STRING_MAX_DRAW) {
                pullBackDistance = DRAW_STRING_MAX_DRAW;
            }

            // //pull the arrow back a bit
            var pullBackOffset = Vec3.multiply(handToNotch, -pullBackDistance);
            var arrowPosition = Vec3.sum(notchPosition, pullBackOffset);

            // // move it forward a bit
            var pushForwardOffset = Vec3.multiply(handToNotch, -ARROW_OFFSET);
            var finalArrowPosition = Vec3.sum(arrowPosition, pushForwardOffset);

            //we draw strings to the rear of the arrow
            this.setArrowRearPosition(finalArrowPosition, arrowRotation);

            //if we're not shooting, we're updating the arrow's orientation
            if (shouldReleaseArrow !== true) {
                Entities.editEntity(this.arrow, {
                    position: finalArrowPosition,
                    rotation: arrowRotation
                });
            }

            //shoot the arrow
            if (shouldReleaseArrow === true && pullBackDistance >= MAX_NEW_ARROW_PULLBACK_DISTANCE) {
                // var arrowProps = Entities.getEntityProperties(this.arrow);

                //scale the shot strength by the distance you've pulled the arrow back and set its release velocity to be
                // in the direction of the v
                var arrowForce = this.scaleArrowShotStrength(pullBackDistance);
                var handToNotchNorm = Vec3.normalize(handToNotch);

                var releaseVelocity = Vec3.multiply(handToNotchNorm, arrowForce);
                // var releaseVelocity2 = Vec3.multiply()

                //make the arrow physical, give it gravity, a lifetime, and set our velocity
                var arrowProperties = {
                    dynamic: true,
                    collisionless: false,
                    collidesWith: "static,dynamic,otherAvatar", // workaround: not with kinematic --> no collision with bow
                    velocity: releaseVelocity,
                    parentID: NULL_UUID,
                    gravity: ARROW_GRAVITY,
                    lifetime: ARROW_LIFETIME,
                    // position: arrowProps.position,
                    // rotation: arrowProps.rotation
                };

                //actually shoot the arrow and play its sound
                Entities.editEntity(this.arrow, arrowProperties);
                this.playShootArrowSound();

                Controller.triggerShortHapticPulse(1, backHand);

                Entities.addAction("travel-oriented", this.arrow, {
                    forward: { x: 0, y: 0, z: -1 },
                    angularTimeScale: 0.1,
                    tag: "arrow from hifi-bow",
                    ttl: ARROW_LIFETIME
                });


            } else if (shouldReleaseArrow === true) {
                // they released without pulling back; just delete the arrow.
                Entities.deleteEntity(this.arrow);
                this.arrow = null;
            }

            if (shouldReleaseArrow === true) {
                //clear the strings back to only the single straight one
                this.deleteStrings();
                Entities.editEntity(this.preNotchString, {
                    visible: true
                });
            }

        },

        scaleArrowShotStrength: function(value) {
            var min1 = SHOT_SCALE.min1;
            var max1 = SHOT_SCALE.max1;
            var min2 = SHOT_SCALE.min2;
            var max2 = SHOT_SCALE.max2;
            return min2 + (max2 - min2) * ((value - min1) / (max1 - min1));
        },

        playStringPullSound: function() {
            var audioProperties = {
                volume: 0.10,
                position: this.bowProperties.position
            };
            this.stringPullInjector = Audio.playSound(this.stringPullSound, audioProperties);
        },

        playShootArrowSound: function(sound) {
            var audioProperties = {
                volume: 0.15,
                position: this.bowProperties.position
            };
            Audio.playSound(this.shootArrowSound, audioProperties);
        },

        playArrowNotchSound: function() {
            var audioProperties = {
                volume: 0.15,
                position: this.bowProperties.position
            };
            Audio.playSound(this.arrowNotchSound, audioProperties);
        },

        changeStringPullSoundVolume: function(pullBackDistance) {
            var audioProperties = {
                volume: this.scaleSoundVolume(pullBackDistance),
                position: this.bowProperties.position
            };

            this.stringPullInjector.options = audioProperties;
        },
        scaleSoundVolume: function(value) {
            var min1 = SHOT_SCALE.min1;
            var max1 = SHOT_SCALE.max1;
            var min2 = 0;
            var max2 = 0.2;
            return min2 + (max2 - min2) * ((value - min1) / (max1 - min1));
        }

    };

    return new Bow();
});
