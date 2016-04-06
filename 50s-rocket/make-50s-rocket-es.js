//
//
//


(function() {
    var _this;
    var utilitiesScript = Script.resolvePath("/~/libraries/utils.js");
    Script.include(utilitiesScript);
    RocketSwitch = function() {
        _this = this;
        this.switchSound = SoundCache.getSound("https://hifi-public.s3.amazonaws.com/sounds/Switches%20and%20sliders/lamp_switch_2.wav");
    };

    RocketSwitch.prototype = {

        findRocket: function(center) {
           var nearbyEntities = Entities.findEntities(center, 100);
            for (i = 0; i < nearbyEntities.length; i++) {
                var nearbyID = nearbyEntities[i];
                var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
                if (nearbyName == '50s rocket') {
                    return nearbyID;
                }
            }
            return null;
        },

        makeRocket: function(center) {
            this.rocketID = Entities.addEntity({
                name: '50s rocket',
                type: 'Model',
                modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket.obj',
                compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-collision-hull.obj',
                collisionsWillMove: false,
                position: center,
                rotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
                script: 'http://headache.hungry.com/~seth/hifi/50s-rocket.js',
                dynamic: true,
                gravity: { x: 0, y: -1.0, z: 0 },
                velocity: { x: 0, y: 0.5, z: 0 }, // to make it fall
                // density: 8000,

                // put center where it is in openscad
                // registrationPoint: { x: 0.5,
                //                      y: 0.0049751, // model height is 20.1, this is (/ 0.1 20.1)
                //                      z: 0.5 },


                userData: JSON.stringify({
                    grabbableKey: { grabbable: false }
                }),
            });

            // see 50s-rocket.js -- Some of these values are copied out of edit.js after this.maintainDoor has been run.
            var doorZDimension = 1.2840; // baseRocketRadius[2] - (baseRocketRadius[0] - rocketWallThickness);
            var registrationPointZ = 0.0769; // rocketWallThickness / doorZDimension
            this.doorID = Entities.addEntity({
                name: '50s rocket door',
                type: 'Model',
                modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.obj',
                compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door-collision-hull.obj',
                dynamic: false,
                gravity: { x: 0, y: 0, z: 0 },
                angularDamping: { x: 0.0, y: 0.0, z: 0.0 },
                parentID: this.rocketID,
                parentJointIndex: -1,
                collidesWith: "static,dynamic,kinematic,myAvatar,otherAvatar",
                script: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.js',
                registrationPoint:  { x: 0.5, y: 0.0, z: registrationPointZ },
                localPosition: { x: 0.587, y: 0.000, z: 3.707 },
                localRoation: { x: 0, y: 0.07845909893512726, z: 0, w: 0.9969173073768616 },
                userData: "{\"grabbableKey\":{\"wantsTrigger\":true}}"
            });
        },

        clickReleaseOnEntity: function(entityID, mouseEvent) {
            if (!mouseEvent.isLeftButton) {
                return;
            }
            this.triggerSwitch();
        },

        startNearTrigger: function() {
            this.triggerSwitch();
        },

        triggerSwitch: function() {
            if (this.findRocket({ x: 165, y: 70, z: 76})) {
                this.signalRocketDoor();
                return;
            }
            var entityIDs = Entities.findEntities(this.position, 40);
            entityIDs.forEach(function(entityID) {
                var props = Entities.getEntityProperties(entityID, ["name"]);
                if (props.name == "50s rocket") {
                    Entities.deleteEntity(entityID);
                }
            });

            Audio.playSound(this.switchSound, {
                volume: 0.5,
                position: this.position
            });

            this.makeRocket({ x: 165, y: 70, z: 76});
        },

        setChannelKey: function(id, params) {
            var newChannelKey = params[0];
            this.channelKey = newChannelKey;
            // print("received new channel key: " + this.channelKey);
            // Messages.subscribe(this.channelKey);
        },

        signalRocketDoor: function() {
            var data = JSON.stringify({
                action: 'door',
                user: MyAvatar.sessionUUID
            });
            // Messages.sendMessage(this.channelKey, data);
            print("rocket door remote clicked, channel = '" + this.channelKey + "'");
            Entities.callEntityMethod(this.channelKey, "handleMessage", [data]);
        },

        preload: function(entityID) {
            this.entityID = entityID;
            this.position = Entities.getEntityProperties(this.entityID, "position").position;
        }
    };

    // entity scripts always need to return a newly constructed object of our type
    return new RocketSwitch();
});
