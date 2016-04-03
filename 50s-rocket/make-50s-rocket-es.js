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

        makeRocket: function(center) {
            Entities.addEntity({
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
        },


        clickReleaseOnEntity: function(entityID, mouseEvent) {
            if (!mouseEvent.isLeftButton) {
                return;
            }
            this.newRocket();
        },

        startNearTrigger: function() {
            this.newRocket();
        },

        newRocket: function() {
            print("");
            print("");
            print("");
            print("newRocket");
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

        flipSwitch: function() {
            var rotation = Entities.getEntityProperties(this.entityID, "rotation").rotation;
            var axis = {
                x: 0,
                y: 1,
                z: 0
            };
            var dQ = Quat.angleAxis(180, axis);
            rotation = Quat.multiply(rotation, dQ);

            Entities.editEntity(this.entityID, {
                rotation: rotation
            });
        },
        preload: function(entityID) {
            this.entityID = entityID;
            this.position = Entities.getEntityProperties(this.entityID, "position").position;
        }
    };

    // entity scripts always need to return a newly constructed object of our type
    return new RocketSwitch();
});