"use strict";

/* global Script, Vec3, Math, Entities, Quat */

(function() {

    var self = this;

    var modelRadius = 4;

    // var sizeScaleForBody = {
    //     "SUN": modelRadius / 400000,
    //     "MOON": modelRadius / 100000,
    //     "default": modelRadius / 100000
    // };

    var sizeScaleForBody = {
        "SUN": modelRadius / 400000,
        "MOON": modelRadius / 50000,
        "default": modelRadius / 50000
    };


    var distanceScaleForOrbit = {
        "NONE": 1,
        "SUN": 5100000000,
        "EARTH": 70000000
    };

    // var speed = 1/12;
    var speed = 1/3600;
    var lifetime = 600;

    var bodyKeys = [
        "SUN",
        "MERCURY",
        "VENUS",
        "EARTH",
        "MOON",
        "MARS",
        "JUPITER",
        "SATURN",
        "URANUS",
        "NEPTUNE",
        "PLUTO",
        "CERES"
    ];


    var toHifiAxis = Quat.fromVec3Degrees({ x: -90, y: 0, z: 0 });
    // var toHifiAxis = { x: 0, y: 0, z: 0, w: 1 };

    var bodyAnchorIDs = {}; // pivots and models are children of this
    var bodyPivotIDs = {}; // each of these has an offset anchor child
    var bodyEntityIDs = {}; // these are children of anchors

    var startTime = null;

    self.preload = function (entityID) {
        self.entityID = entityID;
    };

    function getDefaultSurface() {
        return [{ red: 100, green: 100, blue: 100 }, null];
    }

    function getSunSurface() {
        // return [ { red: 255, green: 255, blue: 0 }, "" ];
        return [{ red: 255, green: 255, blue: 255 },
                JSON.stringify({
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("sun.fs")
                    }
                })];
    }


    function getEarthSurface() {
        return [{ red: 255, green: 255, blue: 255 },
                JSON.stringify({
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
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("spheremap.fs"),
                        channels: [Script.resolvePath(texturePath)]
                    }
                })];
    }


    var surfaceFunctions = {
        "unknown": getDefaultSurface,
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
        if (surfaceFunctions[bodyKey]) {
            return surfaceFunctions[bodyKey]();
        }
        return surfaceFunctions["unknown"];
    }


    function cleanupEntities() {
        var entityIDs = Entities.findEntities(self.orreryBaseLocation, 1000);
        for (var i = 0; i < entityIDs.length; i++) {
            var entityProps = Entities.getEntityProperties(entityIDs[i], ["userData"]);
            try {
                var parsedUserData = JSON.parse(entityProps.userData);
                if (parsedUserData.orrery == true) {
                    Entities.deleteEntity(entityIDs[i]);
                }
            } catch (e) {
            }
        }
    }


    function getBodyPosition(bodies, bodyKey) {
        // distances from sun range from 376632 to 5023876112
        if (bodyKey == "SUN") {
            return self.orreryBaseLocation;
        }

        var bodyData = bodies[bodyKey];
        var distanceScale = modelRadius / distanceScaleForOrbit[bodyData.orbits];
        var position = Vec3.multiply(Vec3.multiplyQbyV(toHifiAxis, bodyData.position), distanceScale);
        return Vec3.sum(getBodyPosition(bodies, bodyData.orbits), position);
    }


    function getBodySize(bodies, bodyKey) {
        // sizes range from 1188 to 695700
        var bodyData = bodies[bodyKey];
        var bodySize = bodyData.size;
        var expValue = 0.65;
        var expSize = {
            x: Math.pow(bodySize.x, expValue),
            y: Math.pow(bodySize.y, expValue),
            z: Math.pow(bodySize.z, expValue)
        };
        var sizeScale = 1.0 / 400.0;
        if (sizeScaleForBody[bodyKey]) {
            sizeScale = sizeScaleForBody[bodyKey];
        } else {
            sizeScale = sizeScaleForBody["default"];
        }
        return Vec3.multiply(expSize, sizeScale);
    }


    function cspiceQuatToEngineeringQuat(q) {
        // ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/q2m_c.html
        // Given an engineering quaternion
        // QENG   = ( q0,  q1,  q2,  q3 )
        // the equivalent SPICE quaternion is
        // QSPICE = ( q3, -q0, -q1, -q2 )

        return { w: q.w, x: -q.x, y: -q.y, z: -q.z };
    }


    function cspiceQuatToHifi(q) {
        return Quat.multiply(toHifiAxis, cspiceQuatToEngineeringQuat(q));
    }


    function apiRequest(func) {

        var now;
        if (!startTime) {
            startTime = Date.now() / 1000;
            now = startTime;
        } else {
            now = Date.now() / 1000;
        }

        var realTimePassed = now - startTime;
        var orreryEpochSeconds = startTime + (realTimePassed * 3600 * speed);

        var orreryWebAPI = "http://headache.hungry.com/~seth/hifi/orrery/orrery-web-api.cgi?time=" + orreryEpochSeconds;
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {

            if (request.readyState === request.DONE && request.status === 200) {
                var response = JSON.parse(request.responseText);
                var bodies = response.bodies;

                func(bodies, response.time);
            }
        };

        request.open('GET', orreryWebAPI);
        request.timeout = 10000;
        request.send();
    }


    function spinBodies(bodies, orreryEpochSeconds) {
        bodyKeys.forEach(function (bodyKey) {
            if (bodyKey == "SUN") {
                // pivotSpin.SUN = { x: 0, y: 0, z: 0 };
                return;
            }

            var bodyData = bodies[bodyKey];

            var rotation = cspiceQuatToHifi(bodyData.orientation);
            var rotationInOneHour = cspiceQuatToHifi(bodyData.orientationInOneHour);
            var bodyAngularVelocity = Quat.safeEulerAngles(Quat.multiply(rotationInOneHour, Quat.inverse(rotation)));
            bodyAngularVelocity = Vec3.multiply(bodyAngularVelocity, speed);

            Entities.editEntity(bodyEntityIDs[bodyKey], {
                // localAngularVelocity: bodyAngularVelocity,
                angularVelocity: bodyAngularVelocity,
            });

            // // Entities.editEntity(bodyAnchorIDs[bodyKey], {
            // //     // XXX spin opposite of pivot?
            // // });

            // var relativePosition = Vec3.multiplyQbyV(toHifiAxis, bodyData.position);
            // var relativePositionInOneHour = Vec3.multiplyQbyV(toHifiAxis, bodyData.positionInOneHour);

            // // bring the relative positions into the frame of the anchor of what this orbits around
            // var parentAnchorProps = Entities.getEntityProperties(bodyAnchorIDs[bodyData.orbits],
            //                                                      ["position", "rotation"]);
            // var basePosition = parentAnchorProps.position;
            // var baseRotation = parentAnchorProps.rotation;
            // var baseMat = Mat4.createFromRotAndTrans(baseRotation, basePosition);
            // var baseMatInv = Mat4.inverse(baseMat);
            // // var baseMatInvRot = Mat4.extractRotation(baseMatInv);

            // var localPosition = Mat4.transformPoint(baseMatInv, relativePosition);
            // var localPositionInOneHour = Mat4.transformPoint(baseMatInv, relativePositionInOneHour);

            // // var radiansChangeInOneHour = Vec3.getAngle(relativePosition, relativePositionInOneHour);
            // var changeInHour = Quat.rotationBetween(localPositionInOneHour, localPosition);
            // var pivotAngularVelocity = Quat.safeEulerAngles(changeInHour);
            // pivotAngularVelocity = Vec3.multiply(pivotAngularVelocity, speed);

            // if (bodyKey == "MOON") {
            //     pivotAngularVelocity = { x: 0, y: 0.1, z: 0 };
            // }

            // Entities.editEntity(bodyPivotIDs[bodyKey], {
            //     localAngularVelocity: pivotAngularVelocity,
            // });
        });
    }


    function createOrrery() {
        var props = Entities.getEntityProperties(self.entityID);
        self.orreryBaseLocation = props.position;
        self.heightAboveFloor = props.localPosition.y - 0.01; // thickness of floor platform divided by 2

        apiRequest(function(bodies, orreryEpochSeconds) {

            bodyKeys.forEach(function(bodyKey) {

                var bodyData = bodies[bodyKey];

                var position = getBodyPosition(bodies, bodyKey); // world-frame
                var rotation = cspiceQuatToHifi(bodyData.orientation);
                var size = getBodySize(bodies, bodyKey);

                var surface = getSurface(bodyKey);
                var color = surface[0];
                var userDataParsed = JSON.parse(surface[1]);

                // var orbitCenterPosition = getBodyPosition(bodies, bodyData.orbits);

                if (bodyPivotIDs[bodyKey]) {
                    Entities.deleteEntity(bodyPivotIDs[bodyKey]);
                }
                var bodyPivotProps = {
                    name: "Orrery " + bodyData.name + " pivot",
                    type: "Box",
                    alpha: 0,
                    color: { blue: 255, green: 255, red: 255 },
                    dimensions: { x: 0.02, y: 0.02, z: 0.02 },
                    dynamic: false,
                    collisionless: true,
                    grab: { grabbable: false },
                    userData: JSON.stringify({ orrery: true }),
                    angularDamping: 0,
                    damping: 0,
                    lifetime: lifetime
                };
                if (bodyKey == "SUN") {
                    bodyPivotProps.position = position;
                    bodyPivotProps.rotation = { x: 0, y: 0, z: 0, w: 1 };
                } else {
                    bodyPivotProps.localPosition = { x: 0, y: 0, z: 0 };
                    bodyPivotProps.rotation = { x: 0, y: 0, z: 0, w: 1 };
                    bodyPivotProps.parentID = bodyAnchorIDs[bodyData.orbits];
                }
                bodyPivotIDs[bodyKey] = Entities.addEntity(bodyPivotProps);


                if (bodyKey == "SUN") {
                    Entities.addEntity({
                        cutoff: 0,
                        damping: 0,
                        dimensions: { x: modelRadius, y: modelRadius, z: modelRadius },
                        falloffRadius: 1000,
                        intensity: 4,
                        name: "Orrery Sun Light",
                        position: self.orreryBaseLocation,
                        parentID: bodyPivotIDs[bodyKey],
                        type: "Light",
                        grab: { grabbable: false },
                        userData: JSON.stringify({ orrery: true }),
                        lifetime: lifetime
                    });
                }


                if (bodyAnchorIDs[bodyKey]) {
                    Entities.deleteEntity(bodyAnchorIDs[bodyKey]);
                }
                bodyAnchorIDs[bodyKey] = Entities.addEntity({
                    name: "Orrery " + bodyData.name + " anchor",
                    type: "Box",
                    alpha: 0,
                    color: { blue: 255, green: 255, red: 255 },
                    dimensions: { x: 0.02, y: 0.02, z: 0.02 },
                    position: position,
                    rotation: { x: 0, y: 0, z: 0, w: 1 },
                    parentID: bodyPivotIDs[bodyKey],
                    dynamic: false,
                    collisionless: true,
                    grab: { grabbable: false },
                    userData: JSON.stringify({ orrery: true }),
                    angularDamping: 0,
                    damping: 0,
                    lifetime: lifetime
                });


                var userData = "{ orrery: true }";
                if (userDataParsed) {
                    userDataParsed.orrery = true;
                    userData = JSON.stringify(userDataParsed);
                }

                if (bodyEntityIDs[bodyKey]) {
                    Entities.deleteEntity(bodyEntityIDs[bodyKey]);
                }
                bodyEntityIDs[bodyKey] = Entities.addEntity({
                    name: "Orrery " + bodyData.name,
                    type: "Sphere",
                    color: color,
                    parentID: bodyAnchorIDs[bodyKey],
                    localPosition: { x: 0, y: 0, z: 0 },
                    rotation: rotation,
                    dimensions: size,
                    collisionless: true,
                    grab: { grabbable: false },
                    userData: userData,
                    angularDamping: 0,
                    damping: 0,
                    lifetime: lifetime
                });

                if (bodyKey == "SATURN") {
                    Entities.addEntity({
                        name: "Orrery Saturn Ring",
                        color: { red: 200, green: 200, blue: 200 },
                        dimensions: { x: size.x * 2, y: modelRadius * 0.0004, z: size.z * 2 },
                        shape: "Cylinder",
                        type: "Shape",
                        dynamic: false,
                        collisionless: true,
                        parentID: bodyEntityIDs[bodyKey],
                        localPosition: { x: 0, y: 0, z: 0 },
                        localRotation: Quat.fromVec3Degrees({ x: 90, y: 0, z: 0 }),
                        grab: { grabbable: false },
                        userData: JSON.stringify({ orrery: true }),
                        lifetime: lifetime
                    });
                }


                var thisHeightAboveFloor = self.heightAboveFloor + position.y - self.orreryBaseLocation.y;

                Entities.addEntity({
                    name: "Orrery " + bodyData.name + " line",
                    type: "Line",
                    color: { red: 255, green: 255, blue: 255 },
                    dimensions: { x: 1, y: 2 * thisHeightAboveFloor, z: 1 },
                    localPosition: { x: 0, y: 0, z: 0 },
                    localRotation: { x: 0, y: 0, z: 0, w: 1 },
                    parentID: bodyAnchorIDs[bodyKey],
                    linePoints: [
                        { x: 0, y: -size.y / 2 - 0.005, z: 0 },
                        { x: 0, y: -thisHeightAboveFloor, z: 0 }
                    ],
                    lifetime: lifetime,
                    lineWidth: 20
                });
            });

            Script.setTimeout(function () {
                spinBodies(bodies, orreryEpochSeconds);
            }, 1000);
            Script.setTimeout(createOrrery, lifetime * 1000 - 5000);
        });
    }


    Script.scriptEnding.connect(function () {
        cleanupEntities();
    });


    Script.setTimeout(createOrrery, 5000);
});
