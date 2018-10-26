"use strict";

/* global Script, Messages, print, Vec3, Math, Entities */

(function() {

    var orreryBaseLocation = { x: 8000, y: 7998, z: 8000 };
    // var bodyOverlays = [];

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
        if (bodyKey == "SUN") {
            return orreryBaseLocation;
        }
        var bodyData = bodies[bodyKey];
        var distanceScale = 200.0 / 5100000000.0;
        var position = Vec3.multiply(bodyData.position, distanceScale);
        return Vec3.sum(getBodyPosition(bodies, bodyData.orbits), position);
    }


    function updateBodies() {
        var orreryWebAPI = "http://headache.hungry.com/~seth/hifi/orrery/orrery-web-api.cgi";
        var request = new XMLHttpRequest();
        request.onreadystatechange = function() {

            if (request.readyState === request.DONE && request.status === 200) {
                var response = JSON.parse(request.responseText);
                var bodies = response.bodies;


                // var mindx = null;
                // var maxdx = null;

                cleanupOverlays();

                for (var bodyKey in bodies) {
                    if (bodies.hasOwnProperty(bodyKey)) {
                        var bodyData = bodies[bodyKey];

                        // if (!mindx || mindx > bodyData.size.x) {
                        //     mindx = bodyData.size.x;
                        // }
                        // if (!maxdx || maxdx < bodyData.size.x) {
                        //     maxdx = bodyData.size.x;
                        // }


                        // positions range from 376632 to 5023876112
                        // var distanceScale = 200.0 / 5100000000.0;
                        // var position = Vec3.multiply(bodyData.position, distanceScale);
                        // position = { x: position.x, y: position.z, z: position.y };
                        // print("position for " + bodyData.name + " = " + JSON.stringify(position));


                        var position = getBodyPosition(bodies, bodyKey);
                        position = { x: position.x, y: position.z, z: position.y }; // fix axis style
                        print("position for " + bodyData.name + " = " + JSON.stringify(position));

                        // sizes range from 1188 to 695700

                        // var sizeScale = 1.0 / 250000.0;
                        // var sizeScale = 1.0 / 4.8;
                        // var logSize = {
                        //     x: Math.log(bodyData.size.x),
                        //     y: Math.log(bodyData.size.y),
                        //     z: Math.log(bodyData.size.z)
                        // };
                        // var expSize = {
                        //     x: Math.pow(bodyData.size.x, 0.1),
                        //     y: Math.pow(bodyData.size.y, 0.1),
                        //     z: Math.pow(bodyData.size.z, 0.1)
                        // };
                        // var size = Vec3.multiply(logSize, sizeScale);


                        var expValue = 0.6;
                        var expSize = {
                            x: Math.pow(bodyData.size.x, expValue),
                            y: Math.pow(bodyData.size.y, expValue),
                            z: Math.pow(bodyData.size.z, expValue)
                        };
                        var sizeScale = 1.0 / 834.0;
                        var size = Vec3.multiply(expSize, sizeScale);

                        // var sizeScale = 1.0 / 120000.0;
                        // var size = Vec3.multiply(bodyData.size, sizeScale);
                        // size = { x: sigmoid(size.x), y: sigmoid(size.y), z: sigmoid(size.z) };

                        // var overlayID = Overlays.addOverlay("sphere", {
                        //     position: Vec3.sum(orreryBaseLocation, position),
                        //     size: size.x,
                        //     color: { red: 30, green: 255, blue: 30 },
                        //     alpha: 1,
                        //     solid: false,
                        //     visible: true,
                        //     drawInFront: false,
                        // });
                        // bodyOverlays.push(overlayID);

                        var color = { blue: 128, green: 128, red: 40 };
                        if (bodyKey == "EARTH") {
                            color = { blue: 128, green: 0, red: 0 };
                        }
                        if (bodyKey == "MARS") {
                            color = { blue: 0, green: 0, red: 255 };
                        }
                        if (bodyKey == "MOON") {
                            color = { blue: 0, green: 255, red: 0 };
                        }

                        var entityID = Entities.addEntity({
                            name: bodyData.name,
                            type: "Sphere",
                            color: color,
                            dimensions: size,
                            position: position,
                            dynamic: false,
                            collisionless: true,
                            lifetime: 120
                        });
                    }
                }

                // print("QQQQ min=" + mindx + " max=" + maxdx);

                // var overlayID = Overlays.addOverlay("sphere", {
                //     position: { x: 0.1, y: 0.1, z: 0.1 },
                //     size: 0.8,
                //     color: { red: 30, green: 255, blue: 30 },
                //     alpha: 1,
                //     solid: false,
                //     visible: true,
                //     drawInFront: false,
                // });

                // var overlayID = Overlays.addOverlay("model", {
                //     url: controller.modelURL,
                //     dimensions: controller.dimensions,
                //     localRotation: controller.rotation,
                //     localPosition: position,
                //     parentID: PARENT_ID,
                //     parentJointIndex: controller.jointIndex,
                //     ignoreRayIntersection: true
                // });
                // bodyOverlays.push(overlayID);

                // print("HERE -- " + JSON.stringify(response));
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

        var parsedMessage = {};
        try {
            parsedMessage = JSON.parse(message);
        }  catch (e) {
            print(e);
        }

        print("[0] Orrery got message: " + JSON.stringify(parsedMessage));
        updateBodies();
    };

    Messages.messageReceived.connect(handleMessages);
    Messages.subscribe("Orrery Controls");

    Script.scriptEnding.connect(function () {
        cleanupOverlays();
    });
});
