
(function() {
    var _this;

    Dropper = function() {
        _this = this;
    }

    Dropper.prototype = {
        needNewRadiationBall: function() {
            var entityIDs = Entities.findEntities(_this.getLocation(), 0.1);
            for (var i = 0; i < entityIDs.length; i++) {
                var entityID = entityIDs[i];
                var props = Entities.getEntityProperties(entityID, ["name"]);
                var name = props.name;
                if (name == "radiation-ball") {
                    return false;
                }
            }
            return true;
        },

        getLocation: function() {
            var dropperID = _this.getEntityID();
            return Entities.getEntityProperties(dropperID, ["position"]).position;
        },

        getEntityID: function() {
            return this.entityID;
        },

        preload: function(entityID) {
            this.entityID = entityID;

            Script.setInterval(function() {
                if (_this.needNewRadiationBall()) {
                    _this.makeRadiationBall();
                }
            }, 10000); // 10 seconds
        },

        unload: function() {
        },

        makeRadiationBall: function() {
            var dropperID = _this.getEntityID();
            var props = Entities.getEntityProperties(dropperID, ["position", "rotation"]);

            var dropperPosition = props.position;
            var dropperRotation = props.rotation;
            var dropperFront = Vec3.multiply(Quat.getRight(dropperRotation), 0.1);
            var ballPosition = Vec3.sum(dropperPosition, dropperFront);

            var ballID = Entities.addEntity({
                "name": "radiation-ball",
                "lifetime": 600,
                "collisionsWillMove": 1,
                "color": {
                    "blue": 129,
                    "green": 227,
                    "red": 118
                },
                restitution: 1.0,
                "dimensions": {
                    "x": 0.065536007285118103,
                    "y": 0.065536007285118103,
                    "z": 0.065536007285118103
                },
                "dynamic": 1,
                "gravity": {
                    "x": 0,
                    "y": -0.5,
                    "z": 0
                },
                "position": ballPosition,
                "type": "Sphere",
                "userData": "{\"grabbableKey\":{\"grabbable\":true}}"
            });
            Entities.addEntity({
                "accelerationSpread": {
                    "x": 0.0099999997764825821,
                    "y": 0,
                    "z": 0.0099999997764825821
                },
                "alpha": 0.10000000149011612,
                "alphaFinish": 0,
                "alphaStart": 0.69999998807907104,
                "azimuthFinish": 0,
                "azimuthStart": 0,
                "color": {
                    "blue": 160,
                    "green": 208,
                    "red": 157
                },
                "colorFinish": {
                    "blue": 160,
                    "green": 208,
                    "red": 157
                },
                "colorStart": {
                    "blue": 160,
                    "green": 208,
                    "red": 157
                },
                "dimensions": {
                    "x": 0.10890001058578491,
                    "y": 0.10890001058578491,
                    "z": 0.10890001058578491
                },
                "emitAcceleration": {
                    "x": 0,
                    "y": 0,
                    "z": 0
                },
                "emitOrientation": {
                    "w": 0.99999994039535522,
                    "x": 0,
                    "y": 0,
                    "z": 0
                },
                "emitRate": 10,
                "emitSpeed": 0,
                "emitterShouldTrail": 1,
                "locked": 1,
                "maxParticles": 30,
                "name": "green glow",
                "parentID": ballID,
                "particleRadius": 0.30000001192092896,
                "radiusFinish": 0.89999997615814209,
                "radiusSpread": 0.029999999329447746,
                "radiusStart": 0.059999998658895493,
                "speedSpread": 0,
                "textures": "https://hifi-public.s3.amazonaws.com/alan/Particles/Particle-Sprite-Smoke-1.png",
                "type": "ParticleEffect",
                "userData": "{\"grabbableKey\":{\"grabbable\":false}}"
            });
        }
    };

    return new Dropper();
});
