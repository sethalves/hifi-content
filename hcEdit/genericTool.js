//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html

/* global Script, module, AvatarManager, Controller, Entities, MyAvatar, Overlays, Quat, SoundCache, Vec3,
   genericTool:true
 */


function genericTool(toolFunctionStart, toolFunctionContinue, toolFunctionStop) {
    Script.include("/~/system/libraries/utils.js");

    var _this;

    var TRIGGER_CONTROLS = [
        Controller.Standard.LT,
        Controller.Standard.RT,
    ];
    var RELOAD_THRESHOLD = 0.3;

    var Tool = function() {
        _this = this;
        this.equipped = false;
        this.forceMultiplier = 1;
        this.laserLength = 100;

        this.activateSound =
            SoundCache.getSound("https://hifi-public.s3.amazonaws.com/sounds/Switches%20and%20sliders/lamp_switch_2.wav");
        this.activateSoundVolume = 0.2;

        this.laserOffsets = { x: 0, y: 0.095, z: 0 };
        this.firingOffsets = { x: 0, y: 0, z: 0.16 };
    };

    Tool.prototype = {
        canActivate: false,

        startEquip: function(id, params) {
            this.equipped = true;
            this.hand = params[0] == "left" ? 0 : 1;
            this.targetEntity = null;
            this.active = false;
            this.canActivate = false; // to avoid firing right as the thing is equipped
            if (this.equipStarted) {
                this.equipStarted(id, params);
            }
        },

        continueEquip: function(id, params) {
            if (!this.equipped) {
                return;
            }
            this.toggleWithTriggerPressure();
            if (this.active && this.toolFunctionContinue) {
                this.updateProps();
                this.toolFunctionContinue();
            }

            if (this.equipContinued) {
                this.equipContinued(id, params);
            }
        },

        updateProps: function() {
            var toolProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
            this.position = toolProperties.position;
            this.rotation = toolProperties.rotation;

            this.firingDirection = Vec3.multiply(-1.0, Quat.getFront(this.rotation));
            var upVec = Quat.getUp(this.rotation);
            this.barrelPoint = Vec3.sum(this.position, Vec3.multiply(upVec, this.laserOffsets.y));
            this.laserTip = Vec3.sum(this.barrelPoint, Vec3.multiply(this.firingDirection, this.laserLength));
            this.barrelPoint = Vec3.sum(this.barrelPoint, Vec3.multiply(this.firingDirection, this.firingOffsets.z));
        },

        toggleWithTriggerPressure: function() {
            this.triggerValue = Controller.getValue(TRIGGER_CONTROLS[this.hand]);

            if (this.triggerValue < RELOAD_THRESHOLD) {
                this.targetEntity = null;
                this.targetAvatar = null;
                this.canActivate = true;
                if (this.active && this.toolFunctionStop) {
                    this.toolFunctionStop();
                }
                this.active = false;
            }
            if (this.canActivate === true && this.triggerValue > 0.55) {
                this.canActivate = false;
                this.updateProps();
                this.activate();
            }
        },

        releaseEquip: function(id, params) {
            if (this.equipStopped) {
                this.equipStopped(id, params);
            }
            this.hand = null;
            this.equipped = false;
            Overlays.editOverlay(this.laser, {
                visible: false
            });
        },

        // triggerPress: function(hand, value) {
        //     if (this.hand === hand && value > 0.5) {
        //         // We are pulling trigger on the hand we have the tool in, so activate
        //         this.activate();
        //     }
        // },

        activate: function() {
            Audio.playSound(this.activateSound, {
                position: this.barrelPoint,
                volume: this.activateSoundVolume
            });

            this.pickRay = {
                origin: this.barrelPoint,
                direction: this.firingDirection
            };

            // print("pickRay direction =" + this.firingDirection.x + " " + this.firingDirection.y + " " + this.firingDirection.z);
            // Entities.addEntity({
            //     type: "Sphere",
            //     position: this.pickRay.origin,
            //     color: {red: 200, green: 0, blue: 0},
            //     dimensions: 0.02,
            //     lifetime: 1.0
            // });
            // Entities.addEntity({
            //     type: "Sphere",
            //     position: Vec3.sum(this.pickRay.origin, this.pickRay.direction),
            //     color: {red: 0, green: 200, blue: 0},
            //     dimensions: 0.02,
            //     lifetime: 1.0
            // });


            // var indicator = Overlays.addOverlay("line3d", {
            //     position: this.pickRay.origin,
            //     start: { x: 0, y: 0, z: 0 },
            //     end: Vec3.multiply(Vec3.normalize(this.pickRay.direction), 3.0),
            //     color: { red: 0, green: 40, blue: 255 },
            //     alpha: 0.8,
            //     solid: true,
            //     visible: true,
            //     ignoreRayIntersection: true,
            //     drawInFront: false
            // });
            // Script.setTimeout(function() {
            //     Overlays.deleteOverlay(indicator);
            // }, 2000);


            var intersection = Entities.findRayIntersection(this.pickRay, true);
            if (intersection.intersects) {
                this.targetEntity = intersection.entityID;
                this.toolActivationProperties = Entities.getEntityProperties(this.entityID);
                this.entityActivationProperties = intersection.properties;
                this.entityDistance = intersection.distance;
            } else {
                this.targetEntity = null;
                this.toolActivationProperties = null;
                this.entityActivationProperties = null;
                this.entityDistance = -1;
            }

            intersection = AvatarManager.findRayIntersection(this.pickRay, [], [MyAvatar.sessionUUID]);
            if (intersection.intersects) {
                this.targetAvatar = intersection.avatarID;
                this.avatarDistance = intersection.distance;
            } else {
                this.targetAvatar = null;
                this.avatarDistance = -1;
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

        makeDebugBall: function() {
            var pos = Vec3.sum(Entities.getEntityProperties(_this.entityID, ["position"]).position, {x:0, y:0.1, z:0});
            _this.debugBall = Entities.addEntity({
                name: "genericTool status",
                type: "Sphere",
                position: pos,
                color: {red: 255, green: 0, blue: 0},
                dimensions: {x:0.06, y:0.06, z:0.06},
                lifetime: 60.0,
                ignoreForCollisions: 1
            });

            if (_this.debugBall == "{00000000-0000-0000-0000-000000000000}") {
                // try again
                Script.setTimeout(_this.makeDebugBall, 250);
            }
        },

        preload: function(entityID) {
            this.entityID = entityID;

            // create debug indicator
            // Script.setTimeout(this.makeDebugBall, 8000);
        },

        unload: function() {
        },
    };

    // entity scripts always need to return a newly constructed object of our type
    return new Tool();
}


module.exports = {
    genericTool: genericTool
};
