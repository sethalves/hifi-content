//
//  Created by Seth Alves on 2016-9-7
//  Copyright 2016 High Fidelity, Inc.
//
//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html
//
//  Previous version by James B. Pollack @imgntn on 10/19/2015

/* global Script, MyAvatar, Vec3, Controller, Quat, Uuid, getControllerWorldLocation,
   SoundCache, Entities, Messages, getEntityCustomData, setEntityCustomData */


function getControllerLocation(controllerHand) {
    var standardControllerValue =
        (controllerHand === "right") ? Controller.Standard.RightHand : Controller.Standard.LeftHand;
    return getControllerWorldLocation(standardControllerValue, true);
}

(function() {

    Script.include("/~/system/libraries/utils.js");
    Script.include("/~/system/libraries/controllers.js");

    var NOTCH_ARROW_SOUND_URL = Script.resolvePath("notch.wav");
    var SHOOT_ARROW_SOUND_URL = Script.resolvePath("String_release2.L.wav");
    var STRING_PULL_SOUND_URL = Script.resolvePath("Bow_draw.1.L.wav");
    var ARROW_HIT_SOUND_URL = Script.resolvePath("Arrow_impact1.L.wav");
    var ARROW_SPARKLE_TEXTURE = Script.resolvePath("arrow-sparkle.png");

    var ARROW_MODEL_URL = Script.resolvePath("arrow.fbx");
    // var ARROW_MODEL_URL = Script.resolvePath("newarrow_textured.fbx");
    var ARROW_DIMENSIONS = {
        x: 0.20,
        y: 0.19,
        z: 0.93
    };

    var MIN_ARROW_DISTANCE_FROM_BOW_REST = 0.2;
    var MAX_ARROW_DISTANCE_FROM_BOW_REST = ARROW_DIMENSIONS.z - 0.2;
    var MIN_ARROW_DISTANCE_FROM_BOW_REST_TO_SHOOT = 0.2;

    var MIN_ARROW_SPEED = 3;
    var MAX_ARROW_SPEED = 30;

    var ARROW_TIP_OFFSET = 0.47;
    var ARROW_GRAVITY = {
        x: 0,
        y: -9.8,
        z: 0
    };


    var ARROW_LIFETIME = 30; // seconds
    var ARROW_PARTICLE_LIFESPAN = 2; // seconds

    var TOP_NOTCH_OFFSET = 0.6;
    var BOTTOM_NOTCH_OFFSET = 0.6;

    var DRAW_STRING_THRESHOLD = 0.80;
    var DRAW_STRING_PULL_DELTA_HAPTIC_PULSE = 0.09;
    var DRAW_STRING_MAX_DRAW = 0.7;
    var NEAR_TO_RELAXED_KNOCK_DISTANCE = 0.5; // if the hand is this close, rez the arrow
    var NEAR_TO_RELAXED_SCHMITT = 0.05;

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

    var STRING_NAME = "Hifi-Bow-String";
    var ARROW_NAME = "Hifi-Arrow-projectile";

    var STATE_IDLE = 0;
    var STATE_ARROW_GRABBED = 1;

    Bow.prototype = {
        topString: null,
        aiming: false,
        arrowTipPosition: null,
        preNotchString: null,
        stringID: null,
        arrow: null,
        stringData: {
            currentColor: {
                red: 255,
                green: 255,
                blue: 255
            }
        },

        state: STATE_IDLE,
        sinceLastUpdate: 0,
        preload: function(entityID) {
            this.entityID = entityID;
            this.stringPullSound = SoundCache.getSound(STRING_PULL_SOUND_URL);
            this.shootArrowSound = SoundCache.getSound(SHOOT_ARROW_SOUND_URL);
            this.arrowHitSound = SoundCache.getSound(ARROW_HIT_SOUND_URL);
            this.arrowNotchSound = SoundCache.getSound(NOTCH_ARROW_SOUND_URL);
            this.stringID = null;
        },

        unload: function() {
            Messages.sendLocalMessage("Hifi-Hand-Disabler", "none");
            Entities.deleteEntity(this.arrow);
        },

        startEquip: function(entityID, args) {
            this.hand = args[0];
            this.bowHand = args[0];
            this.stringHand = this.bowHand === "right" ? "left" : "right";

            Entities.editEntity(_this.entityID, {
                collidesWith: "",
            });

            var data = getEntityCustomData("grabbableKey", this.entityID, {});
            data.grabbable = false;
            setEntityCustomData("grabbableKey", this.entityID, data);

            this.initString();

            var self = this;
            this.updateIntervalID = Script.setInterval(function() { self.update(); }, 11);
        },

        getStringHandPosition: function() {
            return getControllerLocation(this.stringHand).position;
        },

        releaseEquip: function(entityID, args) {
            Script.clearInterval(this.updateIntervalID);
            this.updateIntervalID = null;

            Messages.sendLocalMessage("Hifi-Hand-Disabler", "none");

            var data = getEntityCustomData("grabbableKey", this.entityID, {});
            data.grabbable = true;
            setEntityCustomData("grabbableKey", this.entityID, data);
            Entities.deleteEntity(this.arrow);
            this.resetStringToIdlePosition();
            this.destroyArrow();
            Entities.editEntity(_this.entityID, {
                collidesWith: "static,dynamic,kinematic,otherAvatar,myAvatar"
            });
        },

        update: function(entityID) {
            var self = this;
            self.deltaTime = checkInterval();
            //debounce during debugging -- maybe we're updating too fast?
            if (USE_DEBOUNCE === true) {
                self.sinceLastUpdate = self.sinceLastUpdate + self.deltaTime;

                if (self.sinceLastUpdate > 60) {
                    self.sinceLastUpdate = 0;
                } else {
                    return;
                }
            }

            //invert the hands because our string will be held with the opposite hand of the first one we pick up the bow with
            this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[(this.hand === "left") ? 1 : 0]);

            this.bowProperties = Entities.getEntityProperties(this.entityID, ["position", "rotation"]);
            var notchPosition = this.getNotchPosition(this.bowProperties);
            var stringHandPosition = this.getStringHandPosition();
            var handToNotch = Vec3.subtract(notchPosition, stringHandPosition);
            var pullBackDistance = Vec3.length(handToNotch);

            if (this.state === STATE_IDLE) {
                this.pullBackDistance = 0;

                this.resetStringToIdlePosition();
                //this.deleteStrings();
                if (this.triggerValue >= DRAW_STRING_THRESHOLD &&
                    pullBackDistance < (NEAR_TO_RELAXED_KNOCK_DISTANCE - NEAR_TO_RELAXED_SCHMITT) &&
                    !this.backHandBusy) {
                    //the first time aiming the arrow
                    this.state = STATE_ARROW_GRABBED;
                }
            }

            if (this.state === STATE_ARROW_GRABBED) {
                if (!this.arrow) {
                    var handToDisable = (this.hand === "right" ? "left" : "right");
                    Messages.sendLocalMessage("Hifi-Hand-Disabler", handToDisable);
                    this.playArrowNotchSound();
                    this.arrow = this.createArrow();
                    this.playStringPullSound();
                }

                if (this.triggerValue < DRAW_STRING_THRESHOLD) {
                    // they let go without pulling
                    if (pullBackDistance >= (MIN_ARROW_DISTANCE_FROM_BOW_REST_TO_SHOOT + NEAR_TO_RELAXED_SCHMITT)) {
                        // The arrow has been pulled far enough back that we can release it
                        Messages.sendLocalMessage("Hifi-Hand-Disabler", "none");
                        this.updateArrowPositionInNotch(true, true);
                        this.arrow = null;
                        this.state = STATE_IDLE;
                        this.resetStringToIdlePosition();
                    } else {
                        // The arrow has not been pulled far enough back so we just remove the arrow
                        Messages.sendLocalMessage("Hifi-Hand-Disabler", "none");
                        Entities.deleteEntity(this.arrow);
                        this.arrow = null;
                        this.state = STATE_IDLE;
                        this.resetStringToIdlePosition();
                    }
                } else {
                    this.updateArrowPositionInNotch(false, true);
                    this.updateString();
                }
            }
        },

        destroyArrow: function() {
            var children = Entities.getChildrenIDs(this.entityID);
            children.forEach(function(childID) {
                var childName = Entities.getEntityProperties(childID, ["name"]).name;
                if (childName == ARROW_NAME) {
                    Entities.deleteEntity(childID);
                    // Continue iterating through children in case we've ended up in
                    // a bad state where there are multiple arrows.
                }
            });
        },

        createArrow: function() {
            this.playArrowNotchSound();

            var avatarEntity = !(Entities.canRez() || Entities.canRezTmp());
            var arrow = Entities.addEntity({
                name: ARROW_NAME,
                type: "Model",
                modelURL: ARROW_MODEL_URL,
                shapeType: "simple-compound",
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
            }, avatarEntity);

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

        initString: function() {
            // Delete any left-over strings
            var children = Entities.getChildrenIDs(this.entityID);
            children.forEach(function(childID) {
                Entities.deleteEntity(childID);
            });

            var avatarEntity = !(Entities.canRez() || Entities.canRezTmp());
            this.stringID = Entities.addEntity({
                collisionless: true,
                dimensions: { "x": 5, "y": 5, "z": 5 },
                ignoreForCollisions: 1,
                linePoints: [ { "x": 0, "y": 0, "z": 0 }, { "x": 0, "y": -1.2, "z": 0 } ],
                lineWidth: 5,
                color: { red: 153, green: 102, blue: 51 },
                name: STRING_NAME,
                parentID: this.entityID,
                localPosition: { "x": 0, "y": 0.6, "z": 0.1 },
                localRotation: { "w": 1, "x": 0, "y": 0, "z": 0 },
                type: "Line",
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    }
                })
            }, avatarEntity);

            this.resetStringToIdlePosition();
        },

        // This resets the string to a straight line
        resetStringToIdlePosition: function() {
            Entities.editEntity(this.stringID, {
                linePoints: [ { "x": 0, "y": 0, "z": 0 }, { "x": 0, "y": -1.2, "z": 0 } ],
                lineWidth: 5,
                localPosition: { "x": 0, "y": 0.6, "z": 0.1 },
                localRotation: { "w": 1, "x": 0, "y": 0, "z": 0 },
            });
        },

        updateString: function() {
            var upVector = Quat.getUp(this.bowProperties.rotation);
            var upOffset = Vec3.multiply(upVector, TOP_NOTCH_OFFSET);
            var downVector = Vec3.multiply(-1, Quat.getUp(this.bowProperties.rotation));
            var downOffset = Vec3.multiply(downVector, BOTTOM_NOTCH_OFFSET);
            var backOffset = Vec3.multiply(-0.1, Quat.getFront(this.bowProperties.rotation));

            var topStringPosition = Vec3.sum(this.bowProperties.position, upOffset);
            this.topStringPosition = Vec3.sum(topStringPosition, backOffset);
            var bottomStringPosition = Vec3.sum(this.bowProperties.position, downOffset);
            this.bottomStringPosition = Vec3.sum(bottomStringPosition, backOffset);

            var stringProps = Entities.getEntityProperties(this.stringID, ["position", "rotation"]);
            var handPositionLocal = Vec3.subtract(this.arrowRearPosition, stringProps.position);
            //handPositionLocal = Vec3.subtract(handPositionLocal, { x: 0, y: 0.6, z: 0 });
            handPositionLocal = Vec3.multiplyQbyV(Quat.inverse(stringProps.rotation), handPositionLocal);

            var linePoints = [
                { x: 0, y: 0, z: 0 },
                //{ x: 0, y: -0.6, z: 1 },
                handPositionLocal,
                { x: 0, y: -1.2, z: 0 },
            ];

            Entities.editEntity(this.stringID, {
                linePoints: linePoints,
            });
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

        updateArrowPositionInNotch: function(shouldReleaseArrow, doHapticPulses) {
            //set the notch that the arrow should go through
            var notchPosition = this.getNotchPosition(this.bowProperties);
            //set the arrow rotation to be between the notch and other hand
            var stringHandPosition = this.getStringHandPosition();
            var handToNotch = Vec3.subtract(notchPosition, stringHandPosition);
            var arrowRotation = Quat.rotationBetween(Vec3.FRONT, handToNotch);

            var backHand = this.hand === "left" ? 1 : 0;
            var pullBackDistance = Vec3.length(handToNotch);
            // pulse as arrow is drawn
            if (doHapticPulses &&
                Math.abs(pullBackDistance - this.pullBackDistance) > DRAW_STRING_PULL_DELTA_HAPTIC_PULSE) {
                Controller.triggerHapticPulse(1, 20, backHand);
                this.pullBackDistance = pullBackDistance;
            }
            // this.changeStringPullSoundVolume(pullBackDistance);

            if (pullBackDistance > DRAW_STRING_MAX_DRAW) {
                pullBackDistance = DRAW_STRING_MAX_DRAW;
            }

            var handToNotchDistance = Vec3.length(handToNotch);
            var stringToNotchDistance = Math.max(MIN_ARROW_DISTANCE_FROM_BOW_REST,
                                                 Math.min(MAX_ARROW_DISTANCE_FROM_BOW_REST, handToNotchDistance));
            var offset = Vec3.subtract(notchPosition,
                                       Vec3.multiply(Vec3.normalize(handToNotch),
                                                     stringToNotchDistance - ARROW_DIMENSIONS.z / 2.0));

            var arrowPosition = offset;

            // Set arrow rear position
            var frontVector = Quat.getFront(arrowRotation);
            var frontOffset = Vec3.multiply(frontVector, -ARROW_TIP_OFFSET);
            var arrorRearPosition = Vec3.sum(arrowPosition, frontOffset);
            this.arrowRearPosition = arrorRearPosition;

            //if we're not shooting, we're updating the arrow's orientation
            if (shouldReleaseArrow !== true) {
                Entities.editEntity(this.arrow, {
                    position: arrowPosition,
                    rotation: arrowRotation
                });
            } else {
                //shoot the arrow
                var arrowAge = Entities.getEntityProperties(this.arrow, ["age"]).age;

                //scale the shot strength by the distance you've pulled the arrow back and set its release velocity to be
                // in the direction of the v
                var arrowForce = this.scaleArrowShotStrength(stringToNotchDistance);
                var handToNotchNorm = Vec3.normalize(handToNotch);

                var releaseVelocity = Vec3.multiply(handToNotchNorm, arrowForce);

                //make the arrow physical, give it gravity, a lifetime, and set our velocity
                var arrowProperties = {
                    dynamic: true,
                    collisionless: false,
                    collidesWith: "static,dynamic,otherAvatar", // workaround: not with kinematic --> no collision with bow
                    velocity: releaseVelocity,
                    parentID: Uuid.NULL,
                    gravity: ARROW_GRAVITY,
                    lifetime: arrowAge + ARROW_LIFETIME,
                };

                // add a particle effect to make the arrow easier to see as it flies
                var arrowParticleProperties = {
                    accelerationSpread: { x: 0, y: 0, z: 0 },
                    alpha: 1,
                    alphaFinish: 0,
                    alphaSpread: 0,
                    alphaStart: 0.3,
                    azimuthFinish: 3.1,
                    azimuthStart: -3.14159,
                    color: { red: 255, green: 255, blue: 255 },
                    colorFinish: { red: 255, green: 255, blue: 255 },
                    colorSpread: { red: 0, green: 0, blue: 0 },
                    colorStart: { red: 255, green: 255, blue: 255 },
                    emitAcceleration: { x: 0, y: 0, z: 0 },
                    emitDimensions: { x: 0, y: 0, z: 0 },
                    emitOrientation: { x: -0.7, y: 0.0, z: 0.0, w: 0.7 },
                    emitRate: 0.01,
                    emitSpeed: 0,
                    emitterShouldTrail: 0,
                    isEmitting: 1,
                    lifespan: ARROW_PARTICLE_LIFESPAN,
                    lifetime: ARROW_PARTICLE_LIFESPAN + 1,
                    maxParticles: 1000,
                    name: "arrow-particles",
                    parentID: this.arrow,
                    particleRadius: 0.132,
                    polarFinish: 0,
                    polarStart: 0,
                    radiusFinish: 0.35,
                    radiusSpread: 0,
                    radiusStart: 0.132,
                    speedSpread: 0,
                    textures: ARROW_SPARKLE_TEXTURE,
                    type: "ParticleEffect"
                };

                var avatarEntity = !(Entities.canRez() || Entities.canRezTmp());
                Entities.addEntity(arrowParticleProperties, avatarEntity);

                // actually shoot the arrow
                Entities.editEntity(this.arrow, arrowProperties);

                // play the sound of a shooting arrow
                this.playShootArrowSound();

                // Controller.triggerShortHapticPulse(1, backHand);

                Entities.addAction("travel-oriented", this.arrow, {
                    forward: { x: 0, y: 0, z: -1 },
                    angularTimeScale: 0.1,
                    tag: "arrow from hifi-bow",
                    ttl: ARROW_LIFETIME
                });


            }
        },

        scaleArrowShotStrength: function(value) {
            var pct = (value - MIN_ARROW_DISTANCE_FROM_BOW_REST) / (MAX_ARROW_DISTANCE_FROM_BOW_REST - MIN_ARROW_DISTANCE_FROM_BOW_REST);
            return MIN_ARROW_SPEED + (pct * (MAX_ARROW_SPEED - MIN_ARROW_SPEED)) ;
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
        },

        handleMessages: function(channel, message, sender) {
            if (sender !== MyAvatar.sessionUUID) {
                return;
            }
            if (channel !== "Hifi-Object-Manipulation") {
                return;
            }
            try {
                var data = JSON.parse(message);
                var action = data.action;
                var hand = data.joint;
                var isBackHand = ((_this.hand == "left" && hand == "RightHand") ||
                                  (_this.hand == "right" && hand == "LeftHand"));
                if ((action == "equip" || action == "grab") && isBackHand) {
                    _this.backHandBusy = true;
                }
                if (action == "release" && isBackHand) {
                    _this.backHandBusy = false;
                }
            }  catch (e) {
                print("WARNING: bow.js -- error parsing Hifi-Object-Manipulation message: " + message);
            }
        }
    };

    var bow = new Bow();

    Messages.subscribe("Hifi-Object-Manipulation");
    Messages.messageReceived.connect(bow.handleMessages);

    return bow;
});
