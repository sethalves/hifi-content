"use strict";

/* global Script, Messages, print, Vec3, Math, Entities, Quat, MyAvatar, Mat4 */

(function() {

    var speed = 1/12;

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
        "PLUTO"
    ];

    var orreryBaseLocation = { x: 12000, y: 11998.5, z: 12000 };
    var toHifiAxis = Quat.fromVec3Degrees({ x: -90, y: 0, z: 0 });
    // var toHifiAxis = { x: 0, y: 0, z: 0, w: 1 };

    var bodyAnchorIDs = {}; // pivots and models are children of this
    var bodyPivotIDs = {}; // each of these has an offset anchor child
    var bodyEntityIDs = {}; // these are children of anchors
    var clockEntityID = null;

    var startTime = null;

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

    function cleanupEntities() {
        var entityIDs = Entities.findEntities(orreryBaseLocation, 1000);
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

        bodyAnchorIDs = {};
        bodyPivotIDs = {};
        bodyEntityIDs = {};
    }

    function getBodyPosition(bodies, bodyKey) {
        // distances from sun range from 376632 to 5023876112
        if (bodyKey == "SUN") {
            return orreryBaseLocation;
        }

        var distanceScaleForOrbit = {
            "NONE": 1,
            "SUN": 5100000000,
            "EARTH": 150000000
        };

        var modelRadius = 250;

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
        var sizeScaleForBody = {
            "SUN": 1 / 1600,
            // "MOON": 1 / 600
            "MOON": 1 / 400
        };
        var sizeScale = 1.0 / 400.0;
        if (sizeScaleForBody[bodyKey]) {
            sizeScale = sizeScaleForBody[bodyKey];
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
        // print("orreryWebAPI = " + orreryWebAPI);
        // var orreryWebAPI = "http://headache.hungry.com/~seth/hifi/orrery/orrery-web-api.cgi";
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
        bodyKeys.forEach(function(bodyKey) {
            if (bodyKey == "SUN") {
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

            // Entities.editEntity(bodyAnchorIDs[bodyKey], {
            //     // XXX spin opposite of pivot?
            // });

            var relativePosition = Vec3.multiplyQbyV(toHifiAxis, bodyData.position);
            var relativePositionInOneHour = Vec3.multiplyQbyV(toHifiAxis, bodyData.positionInOneHour);

            // bring the relative positions into the frame of the anchor of what this orbits around
            var parentAnchorProps = Entities.getEntityProperties(bodyAnchorIDs[bodyData.orbits],
                                                                 ["position", "rotation"]);
            var basePosition = parentAnchorProps.position;
            var baseRotation = parentAnchorProps.rotation;
            var baseMat = Mat4.createFromRotAndTrans(baseRotation, basePosition);
            var baseMatInv = Mat4.inverse(baseMat);
            // var baseMatInvRot = Mat4.extractRotation(baseMatInv);

            var localPosition = Mat4.transformPoint(baseMatInv, relativePosition);
            var localPositionInOneHour = Mat4.transformPoint(baseMatInv, relativePositionInOneHour);

            // var radiansChangeInOneHour = Vec3.getAngle(relativePosition, relativePositionInOneHour);
            var changeInHour = Quat.rotationBetween(localPositionInOneHour, localPosition);
            var pivotAngularVelocity = Quat.safeEulerAngles(changeInHour);
            pivotAngularVelocity = Vec3.multiply(pivotAngularVelocity, speed);

            Entities.editEntity(bodyPivotIDs[bodyKey], {
                localAngularVelocity: pivotAngularVelocity,
            });
        });

        // Script.setInterval(function() {
        //     for (var bodyKey in bodyKeys) {
        //         if (bodies.hasOwnProperty(bodyKey)) {
        //             // var bodyData = bodies[bodyKey];
        //             var rotation = Quat.slerp(rot0s[bodyKey], rot1s[bodyKey], spins[bodyKey]);
        //             spins[bodyKey] += 0.5;
        //             Entities.editEntity(bodyEntityIDs[bodyKey], {
        //                 rotation: rotation
        //             });
        //         }
        //     }
        // }, 300);


        updateClock(orreryEpochSeconds);
    }

    function updateBodies() {
        apiRequest(function(bodies, orreryEpochSeconds) {
            bodyKeys.forEach(function(bodyKey) {
                var bodyData = bodies[bodyKey];

                var position = getBodyPosition(bodies, bodyKey);
                var rotation = cspiceQuatToHifi(bodyData.orientation);
                var size = getBodySize(bodies, bodyKey);

                var surface = getSurface(bodyKey);
                var color = surface[0];
                var userDataParsed = JSON.parse(surface[1]);

                // var orbitCenterPosition = getBodyPosition(bodies, bodyData.orbits);

                if (bodyPivotIDs[bodyKey]) {
                    // Entities.editEntity(bodyPivotIDs[bodyKey], {
                    //     rotation: rotation
                    // });
                } else {
                    bodyPivotIDs[bodyKey] = Entities.addEntity({
                        name: "Orrery " + bodyData.name + " pivot",
                        type: "Box",
                        color: { blue: 255, green: 255, red: 255 },
                        dimensions: { x: 0.02, y: 0.02, z: 0.02 },
                        localPosition: { x: 0, y: 0, z: 0 },
                        // rotation: { x: 0, y: 0, z: 0, w: 1 },
                        parentID: bodyAnchorIDs[bodyData.orbits],
                        dynamic: false,
                        collisionless: true,
                        userData: JSON.stringify({
                            grabbableKey: { grabbable: false },
                            orrery: true
                        }),
                        angularDamping: 0,
                        damping: 0,
                        lifetime: 600
                    });

                    Entities.editEntity(bodyPivotIDs[bodyKey], {
                        rotation: { x: 0, y: 0, z: 0, w: 1 }
                    });
                }

                if (bodyAnchorIDs[bodyKey]) {
                    Entities.editEntity(bodyAnchorIDs[bodyKey], {
                        position: position
                    });
                } else {
                    bodyAnchorIDs[bodyKey] = Entities.addEntity({
                        name: "Orrery " + bodyData.name + " anchor",
                        type: "Box",
                        color: { blue: 255, green: 255, red: 255 },
                        dimensions: { x: 0.02, y: 0.02, z: 0.02 },
                        position: position,
                        // rotation: { x: 0, y: 0, z: 0, w: 1 },
                        parentID: bodyPivotIDs[bodyKey],
                        dynamic: false,
                        collisionless: true,
                        userData: JSON.stringify({
                            grabbableKey: { grabbable: false },
                            orrery: true
                        }),
                        angularDamping: 0,
                        damping: 0,
                        lifetime: 600
                    });

                    Entities.editEntity(bodyAnchorIDs[bodyKey], {
                        rotation: { x: 0, y: 0, z: 0, w: 1 }
                    });
                }

                userDataParsed.orrery = true;
                userDataParsed.grabbableKey = { grabbable: false };
                var userData = JSON.stringify(userDataParsed);

                if (bodyEntityIDs[bodyKey]) {
                    Entities.editEntity(bodyEntityIDs[bodyKey], {
                        rotation: rotation
                    });
                } else {
                    bodyEntityIDs[bodyKey] = Entities.addEntity({
                        name: "Orrery " + bodyData.name,
                        type: "Sphere",
                        color: color,

                        parentID: bodyAnchorIDs[bodyKey],
                        localPosition: { x: 0, y: 0, z: 0 },
                        // position: position,
                        rotation: rotation,

                        dimensions: size,
                        collisionless: true,
                        userData: userData,
                        angularDamping: 0,
                        damping: 0,
                        lifetime: 600
                    });

                    if (bodyKey == "SATURN") {
                        Entities.addEntity({
                            name: "Orrery Saturn Ring",
                            color: { red: 200, green: 200, blue: 200 },
                            dimensions: { x: 6, y: 0.1, z: 6 },
                            shape: "Cylinder",
                            type: "Shape",
                            dynamic: false,
                            collisionless: true,
                            // parentID: bodyEntityIDs[bodyKey],
                            parentID: bodyAnchorIDs[bodyKey],
                            localPosition: { x: 0, y: 0, z: 0 },
                            localRotation: Quat.fromVec3Degrees({ x: -90, y: 0, z: 0 }),
                            userData: JSON.stringify({
                                grabbableKey: { grabbable: false },
                                orrery: true
                            })
                        });
                    }


                    // Entities.editEntity(bodyEntityIDs[bodyKey], {
                    //     // rotation: rotation
                    //     parentID: bodyAnchorIDs[bodyKey],
                    //     localPosition: { x: 0, y: 0, z: 0 },
                    // });
                }
            });

            updateClock(orreryEpochSeconds);
        });
    }


    function updateClock(orreryEpochSeconds) {
        if (!clockEntityID) {
            var entityIDs = Entities.findEntities(orreryBaseLocation, 1000);
            var fastForwardButtonID = null;
            for (var i = 0; i < entityIDs.length; i++) {
                var entityProps = Entities.getEntityProperties(entityIDs[i], ["userData", "name"]);
                if (entityProps && entityProps.name == "Orrery Fast Forward Button") {
                    fastForwardButtonID = entityIDs[i];
                }

                if (entityProps && entityProps.name == "Orrery Date/Time") {
                    clockEntityID = entityIDs[i];
                    break;
                }
            }

            clockEntityID = Entities.addEntity({
                name: "Orrery Date/Time",
                type: "Text",
                text: "",
                parentID: fastForwardButtonID,
                localPosition: { x: 0, y: 0.5, z: 0 },
                localRotation: { x: 0, y: 0, z: 0, w: 1 },
                dimensions: { x: 0.5, y: 0.2, z: 0.5 },
                collisionless: true,
                userData: JSON.stringify({
                    grabbableKey: { grabbable: false },
                    orrery: true
                }),
                lifetime: 600
            });
        }

        print("orreryEpochSeconds=" + orreryEpochSeconds);
        var orreryDate = new Date(orreryEpochSeconds * 1000);
        var clockString = orreryDate.toGMTString();
        print("clockString=" + clockString);

        Entities.editEntity(clockEntityID, { text: orreryEpochSeconds });
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
            if (parsedMessage.action == "reset") {
                startTime = null;
                cleanupEntities();
                updateBodies();
            } else if (parsedMessage.action == "spin") {
                startTime = null;
                cleanupEntities();
                updateBodies();
                apiRequest(spinBodies);
            }
        } catch (e) {
            print(e);
        }
    };

    Messages.messageReceived.connect(handleMessages);
    Messages.subscribe("Orrery Controls");

    Script.scriptEnding.connect(function () {
        cleanupEntities();
    });
});
