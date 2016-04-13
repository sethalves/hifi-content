//
//
//

//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html
/*global print, MyAvatar, Entities, AnimationCache, SoundCache, Scene, Camera, Overlays, Audio, HMD, AvatarList, AvatarManager, Controller, UndoStack, Window, Account, GlobalServices, Script, ScriptDiscoveryService, LODManager, Menu, Vec3, Quat, AudioDevice, Paths, Clipboard, Settings, XMLHttpRequest, randFloat, randInt */


genericTool = function (toolFunctionStart, toolFunctionContinue, toolFunctionStop) {
    Script.include("/~/libraries/utils.js");
    Script.include("/~/libraries/constants.js");

    var TRIGGER_CONTROLS = [
        Controller.Standard.LT,
        Controller.Standard.RT,
    ];
    var RELOAD_THRESHOLD = 0.95;

    Tool = function() {
        _this = this;
        this.equipped = false;
        this.forceMultiplier = 1;
        this.laserLength = 100;

        this.activateSound =
            SoundCache.getSound("https://hifi-public.s3.amazonaws.com/sounds/Switches%20and%20sliders/lamp_switch_2.wav");
        this.activateSoundVolume = 0.2;

        this.laserOffsets = {
            y: 0.095
        };
        this.firingOffsets = {
            z: 0.16
        }
    };

    Tool.prototype = {
        canActivate: false,

        startEquip: function(id, params) {
            this.equipped = true;
            this.hand = params[0] == "left" ? 0 : 1;
            this.targetEntity = null;
            this.active = false;
        },

        continueEquip: function(id, params) {
            if (!this.equipped) {
                return;
            }
            this.updateProps();
            this.toggleWithTriggerPressure();
            if (this.active && this.toolFunctionContinue) {
                this.toolFunctionContinue();
            }
        },

        updateProps: function() {
            var toolProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
            this.position = toolProperties.position;
            this.rotation = toolProperties.rotation;

            this.firingDirection = Quat.getFront(this.rotation);
            var upVec = Quat.getUp(this.rotation);
            this.barrelPoint = Vec3.sum(this.position, Vec3.multiply(upVec, this.laserOffsets.y));
            this.laserTip = Vec3.sum(this.barrelPoint, Vec3.multiply(this.firingDirection, this.laserLength));
            this.barrelPoint = Vec3.sum(this.barrelPoint, Vec3.multiply(this.firingDirection, this.firingOffsets.z))
        },

        toggleWithTriggerPressure: function() {
            this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[this.hand]);

            if (this.triggerValue < RELOAD_THRESHOLD) {
                this.targetEntity = null;
                this.canActivate = true;
                if (this.active && this.toolFunctionStop) {
                    this.toolFunctionStop();
                }
                this.active = false;
            }
            if (this.canActivate === true && this.triggerValue === 1) {
                this.canActivate = false;
                this.activate();
            }
        },

        releaseEquip: function(id, params) {
            this.hand = null;
            this.equipped = false;
            Overlays.editOverlay(this.laser, {
                visible: false
            });
        },

        triggerPress: function(hand, value) {
            if (this.hand === hand && value === 1) {
                // We are pulling trigger on the hand we have the tool in, so activate
                this.activate();
            }
        },

        activate: function() {
            Audio.playSound(this.activateSound, {
                position: this.barrelPoint,
                volume: this.activateSoundVolume
            });

            var pickRay = {
                origin: this.barrelPoint,
                direction: this.firingDirection
            };

            var intersection = Entities.findRayIntersectionBlocking(pickRay, true);
            if (intersection.intersects) {
                this.targetEntity = intersection.entityID;
                this.toolActivationProperties = Entities.getEntityProperties(this.entityID);
                this.entityActivationProperties = intersection.properties;
            } else {
                this.targetEntity = null;
                this.toolActivationProperties = null;
                this.entityActivationProperties = null;
            }
            this.active = true;
            if (this.toolFunctionStart) {
                this.toolFunctionStart();
            }
        },

        toolFunctionStart: toolFunctionStart,
        toolFunctionContinue: toolFunctionContinue,
        toolFunctionStop: toolFunctionStop,

        getEntityID: function() {
            return this.entityID;
        },

        preload: function(entityID) {
            this.entityID = entityID;
        },

        unload: function() {
        },
    };

    // entity scripts always need to return a newly constructed object of our type
    return new Tool();
};
