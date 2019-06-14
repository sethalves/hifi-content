
"use strict";

/* global Script, Entities, Messages, Uuid */

(function() {

    var self = this;
    var lampID = "{adff73c5-56ae-4409-a28f-53bec0606e26}";

    self.preload = function (entityID) {
        lampID = entityID;
    };


    function isNullID(testID) {
        if (!testID) {
            return true;
        } else if (testID == Uuid.NULL) {
            return true;
        } else {
            return false;
        }
    }


    function getLightID() {
        var childIDs = Entities.getChildrenIDs(lampID);
        if (childIDs && !isNullID(childIDs[0])) {
            return childIDs[0];
        }
        return null;
    }


    function rezLight() {
        var bulbID = Entities.addEntity({
            name: "lamp light",
            type: "Sphere",
            color: { red: 255, green: 255, blue: 255 },
            dimensions: 0.09,
            localPosition: { x: 0.286, y: -0.1, z: 0 },
            dynamic: false,
            collisionless: true,
            lifetime: 30 * 60,
            alpha: 0.6,
            parentID: lampID,
            parentJointIndex: -1,
            ignorePickIntersection: true,
            grab: { grabbable: true, grabDelegateToParent: false }
        });

        Entities.addEntity({
            name: "lamp light",
            "type": "Light",
            localPosition: { x: 0, y: 0, z: 0 },
            dynamic: false,
            collisionless: true,
            parentID: bulbID,
            parentJointIndex: -1,
            ignorePickIntersection: true,
            grab: { grabbable: true },
            dimensions: { x: 20, y: 20, z: 20 },
            intensity: 3,
            exponent: 1,
            cutoff: 75,
            falloffRadius: 20
        });
    }


    function deleteLight() {
        var bulbID = getLightID();
        Entities.deleteEntity(bulbID);
    }


    function handleMessages(channel, message, sender) {
        // if (sender !== MyAvatar.sessionUUID) {
        //     return;
        // }
        if (channel !== "Day-Night-Cycle") {
            return;
        }
        var data;
        try {
            data = JSON.parse(message);
        } catch (e) {
            print("WARNING: error parsing \"Day-Night-Cycle\" message: " + message);
            return;
        }

        var method = data.method;
        if (method == "set-cycle-stage") {
            var cycleStage = data.value;
            if (cycleStage >= 0 && cycleStage < 9) {

                var lightID = getLightID();
                if (cycleStage > 3) {
                    // dark outside
                    if (!lightID) {
                        rezLight();
                    }
                } else {
                    // light outside
                    if (lightID) {
                        deleteLight();
                    }
                }

            } else {
                print("WARNING: error bad \"Day-Night-Cycle\" message: " + message);
            }
        }
    }

    function cleanup() {
        Messages.unsubscribe("Day-Night-Cycle");
        Messages.messageReceived.disconnect(handleMessages);
    }


    function startup() {
        Script.scriptEnding.connect(cleanup);
        Messages.messageReceived.connect(handleMessages);
        Messages.subscribe("Day-Night-Cycle");
    }

    startup();

});
