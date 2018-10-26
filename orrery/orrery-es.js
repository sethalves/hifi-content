"use strict";

/* global Script, Messages, print, Vec3, Math, Entities, Quat, MyAvatar */

(function() {

    var orreryBaseLocation = { x: 8000, y: 7999, z: 8000 };

    var bodyEntityIDs = {};

    function notYet() {
        return [ { red: 90, green: 90, blue: 90 },
                 JSON.stringify({
                     grabbableKey: {
                         grabbable: false
                     }
                 })];
    }

    function getSunSurface() {
        // return [ { red: 255, green: 255, blue: 0 }, "" ];
        return [{ red: 255, green: 255, blue: 255 },
                JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    },
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("sun.fs")
                    }
                })];
    }

    function getEarthSurface() {
        return [{ red: 255, green: 255, blue: 255 },
                JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    },
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("spheremap.fs"),
                        channels: [Script.resolvePath("models/Earth/Albedo.jpg")]
                    }
                })];
    }

    function getSimpleTextureSurface(texturePath) {
        return [{ red: 255, green: 255, blue: 255 },
                JSON.stringify({
                    grabbableKey: {
                        grabbable: false
                    },
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("spheremap.fs"),
                        channels: [Script.resolvePath(texturePath)]
                    }
                })];
    }

    var surfaceFunctions = {
        "SUN": getSunSurface,
        "MERCURY": function() { return getSimpleTextureSurface("models/Mercury/mercurymap.jpg"); },
        "VENUS": function() { return getSimpleTextureSurface("models/Venus/venusmap.jpg"); },
        "EARTH": getEarthSurface,
        "MOON": function() { return getSimpleTextureSurface("models/Moon/MoonColorMap.png"); },
        "MARS": function() { return getSimpleTextureSurface("models/Mars/mars_1k_color.jpg"); },
        "JUPITER": function() { return getSimpleTextureSurface("models/Jupiter/jupitermap.jpg"); },
        "SATURN": function() { return getSimpleTextureSurface("models/Saturn/saturnmap.jpg"); },
        "URANUS": function() { return getSimpleTextureSurface("models/Uranus/uranusmap.jpg"); },
        "NEPTUNE": function() { return getSimpleTextureSurface("models/Neptune/neptunemap.jpg"); },
        "PLUTO": function() { return getSimpleTextureSurface("models/Pluto/plutomap2k.jpg"); }
    };

    function getSurface(bodyKey) {
        return surfaceFunctions[bodyKey]();
    }

    function cleanupOverlays() {
        // for (var i = 0; i < bodyOverlays.length; i++) {
        //     Overlays.deleteOverlay(bodyOverlays[i]);
        // }
        // bodyOverlays = [];
    }

    function sigmoid(t) {
        return 1/(1+Math.pow(Math.E, -t));
    }

    function getBodyPosition(bodies, bodyKey) {
        // distances from sun range from 376632 to 5023876112
        if (bodyKey == "SUN") {
            return orreryBaseLocation;
        }

        var distanceScaleForOrbit = {
            "NONE": 1,
            "SUN": 5100000000,
            "EARTH": 1200000000
        };

        var modelRadius = 250;

        var bodyData = bodies[bodyKey];
        var distanceScale = modelRadius / distanceScaleForOrbit[bodyData.orbits];
        var position = Vec3.multiply(bodyData.position, distanceScale);
        var result = Vec3.sum(getBodyPosition(bodies, bodyData.orbits), position);
        return { x: result.x, y: result.z, z: result.y };  // fix axis style
    }

    function getBodySize(bodies, bodyKey) {
        // sizes range from 1188 to 695700
        var bodyData = bodies[bodyKey];
        var expValue = 0.65;
        var expSize = {
            x: Math.pow(bodyData.size.x, expValue),
            y: Math.pow(bodyData.size.y, expValue),
            z: Math.pow(bodyData.size.z, expValue)
        };
        var sizeScaleForBody = {
            "SUN": 1 / 1600
        };
        var sizeScale = 1.0 / 400.0;
        if (sizeScaleForBody[bodyKey]) {
            sizeScale = sizeScaleForBody[bodyKey];
        }
        var result = Vec3.multiply(expSize, sizeScale);
        return { x: result.x, y: result.z, z: result.y };  // fix axis style
    }

    function updateBodies() {
        var orreryWebAPI = "http://headache.hungry.com/~seth/hifi/orrery/orrery-web-api.cgi";
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {

            if (request.readyState === request.DONE && request.status === 200) {
                var response = JSON.parse(request.responseText);
                var bodies = response.bodies;

                cleanupOverlays();

                for (var bodyKey in bodies) {
                    if (bodies.hasOwnProperty(bodyKey)) {
                        var bodyData = bodies[bodyKey];

                        var position = getBodyPosition(bodies, bodyKey);
                        var size = getBodySize(bodies, bodyKey);

                        var surface = getSurface(bodyKey);
                        var color = surface[0];
                        var userData = surface[1];

                        bodyEntityIDs[bodyKey] = Entities.addEntity({
                            name: bodyData.name,
                            type: "Sphere",
                            color: color,
                            position: position,
                            rotation: Quat.fromVec3Degrees({ x: 0, y: 0, z: 0 }),
                            dimensions: size,
                            collisionless: true,
                            userData: userData,
                            lifetime: 60
                        });
                    }
                }
            }
        };

        request.open('GET', orreryWebAPI);
        request.timeout = 10000;
        request.send();
    }

    var handleMessages = function(channel, message, sender) {
        if (channel !== "Orrery Controls") {
            return;
        }
        if (sender != MyAvatar.sessionUUID) {
            return;
        }

        var parsedMessage = {};
        try {
            parsedMessage = JSON.parse(message);
            print("[0] Orrery got message: " + JSON.stringify(parsedMessage));
            updateBodies();
        }  catch (e) {
            print(e);
        }
    };

    Messages.messageReceived.connect(handleMessages);
    Messages.subscribe("Orrery Controls");

    Script.scriptEnding.connect(function () {
        cleanupOverlays();
    });
});
