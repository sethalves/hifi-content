var SCALE = 3.0;
var EARTH_SIZE = 3.959 / SCALE;
var MOON_SIZE = 1.079 / SCALE;


(function () {
    this.preload = function (entityID) {
        var position = Entities.getEntityProperties(entityID, ["position"]).position;

        Entities.addEntity({
            type: "Model",
            name: "Earth",
            modelURL: "https://s3-us-west-1.amazonaws.com/hifi-content/seth/production/NBody/earth.fbx",
            position: position,
            dimensions: { x: EARTH_SIZE, y: EARTH_SIZE, z: EARTH_SIZE },
            shapeType: "sphere",
            lifetime: 86400, // 1 day
            angularDamping: 0,
            angularVelocity: { x: 0, y: 0.1, z: 0 },
        });

        var moon = Entities.addEntity({
            type: "Model",
            name: "Moon",
            modelURL: "https://s3-us-west-1.amazonaws.com/hifi-content/seth/production/NBody/moon.fbx",
            position: Vec3.sum(position, { x: EARTH_SIZE, y: 0, z: 0 }),
            dimensions: { x: MOON_SIZE, y: MOON_SIZE, z: MOON_SIZE },
            dynamic: true,
            damping: 0, // 0.01,
            angularDamping: 0, // 0.01,
            script: "https://s3-us-west-1.amazonaws.com/hifi-content/seth/production/NBody/gravity.js",
            lifetime: 86400, // 1 day
            shapeType: "sphere"
        });

        Entities.addEntity({
            "accelerationSpread": {
                "x": 0,
                "y": 0,
                "z": 0
            },
            "alpha": 1,
            "alphaFinish": 0,
            "alphaStart": 1,
            "azimuthFinish": 0,
            "azimuthStart": 0,
            "color": {
                "blue": 255,
                "green": 255,
                "red": 255
            },
            "colorFinish": {
                "blue": 255,
                "green": 255,
                "red": 255
            },
            "colorStart": {
                "blue": 255,
                "green": 255,
                "red": 255
            },
            "dimensions": {
                "x": 0.1089,
                "y": 0.1089,
                "z": 0.1089
            },
            "emitAcceleration": {
                "x": 0,
                "y": 0,
                "z": 0
            },
            "emitOrientation": {
                "w": 1,
                "x": 0,
                "y": 0,
                "z": 0
            },
            "emitRate": 300,
            "emitSpeed": 0,
            "emitterShouldTrail": 1,
            "maxParticles": 10000,
            "name": "moon trail",
            "parentID": moon,
            "particleRadius": 0.005,
            "radiusFinish": 0.005,
            "radiusSpread": 0.005,
            "radiusStart": 0.005,
            "speedSpread": 0,
            "lifespan": 20,
            "textures": "https://hifi-public.s3.amazonaws.com/alan/Particles/Particle-Sprite-Smoke-1.png",
            "type": "ParticleEffect",
            "userData": "{\"grabbableKey\":{\"grabbable\":false}}",
            lifetime: 86400 // 1 day
        });

        // remove self
        Entities.deleteEntity(entityID);
    }
})
