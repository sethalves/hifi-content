"use strict";

/* global Entities, Script, Tablet, MyAvatar, Vec3, Messages */

(function() { // BEGIN LOCAL_SCOPE
    Script.include("/~/system/libraries/utils.js");
    // Script.include("/~/system/libraries/Xform.js");

    var BUGGY_UI_URL = Script.resolvePath("buggy.html");
    // var DEG_TO_RAD = Math.PI / 180.0;
    // var RAD_TO_DEG = 180.0 / Math.PI;
    var DEFAULT_BUGGY_SIZE = { x: 2, y: 0.25, z: 2.8 };

    var lifetime = 7200;
    var speed = 0.0;
    var impulse = 4.0;

    var carBodyID;
    var wheel0ID;
    var wheel1ID;
    var wheel2ID;
    var wheel3ID;
    var steeringLeverID;
    var wheel0Constraint;
    var wheel1Constraint;
    var wheel2Constraint;
    var wheel3Constraint;
    var steeringLeverConstraint;

    var avatarLocalPosition;
    var avatarLocalRotation;

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: Script.resolvePath("buggy.svg"),
        text: "Buggy",
        sortOrder: 15
    });


    function newBuggy(params) {
        speed = params.speed;
        impulse = params.impulse;

        var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.1, z: -4}));
        var bodyDimensions = {
            x: params['buggy-width'],
            y: params['buggy-height'],
            z: params['buggy-length']
        };

        var wheelRadius = 0.3;
        var wheelGap = 0.03;
        var wheel0Offset = { x: bodyDimensions.x / 2,
                             y: bodyDimensions.y / -2 - wheelRadius - wheelGap,
                             z: bodyDimensions.z / 2 };
        var wheel1Offset = { x: -bodyDimensions.x / 2,
                             y: bodyDimensions.y / -2 - wheelRadius - wheelGap,
                             z: bodyDimensions.z / 2 };
        var wheel2Offset = { x: bodyDimensions.x / 2,
                             y: bodyDimensions.y / -2 - wheelRadius - wheelGap,
                             z: -bodyDimensions.z / 2 };
        var wheel3Offset = { x: -bodyDimensions.x / 2,
                             y: bodyDimensions.y / -2 - wheelRadius - wheelGap,
                             z: -bodyDimensions.z / 2 };

        var steeringLeverLength = 1.3;
        var steeringLeverOffset = { x: 0,
                                    y: steeringLeverLength / 2.0 + bodyDimensions.y / 2.0 + 0.1,
                                    z: bodyDimensions.z / 4.0 };

        carBodyID = Entities.addEntity({
            name: "hinge test car body",
            type: "Box",
            color: { blue: 128, green: 40, red: 20 },
            dimensions: bodyDimensions,
            position: pos,
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });

        wheel0ID = Entities.addEntity({
            name: "hinge test wheel 0",
            type: "Sphere",
            color: { blue: 40, green: 200, red: 20 },
            dimensions: { x: wheelRadius * 2, y: wheelRadius * 2, z: wheelRadius * 2 },
            position: Vec3.sum(pos, wheel0Offset),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            friction: 5.0,
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });
        wheel1ID = Entities.addEntity({
            name: "hinge test wheel 1",
            type: "Sphere",
            color: { blue: 40, green: 200, red: 20 },
            dimensions: { x: wheelRadius * 2, y: wheelRadius * 2, z: wheelRadius * 2 },
            position: Vec3.sum(pos, wheel1Offset),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            friction: 5.0,
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });
        wheel2ID = Entities.addEntity({
            name: "hinge test wheel 1",
            type: "Sphere",
            color: { blue: 40, green: 200, red: 20 },
            dimensions: { x: wheelRadius * 2, y: wheelRadius * 2, z: wheelRadius * 2 },
            position: Vec3.sum(pos, wheel2Offset),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            friction: 5.0,
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });
        wheel3ID = Entities.addEntity({
            name: "hinge test wheel 1",
            type: "Sphere",
            color: { blue: 40, green: 200, red: 20 },
            dimensions: { x: wheelRadius * 2, y: wheelRadius * 2, z: wheelRadius * 2 },
            position: Vec3.sum(pos, wheel3Offset),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            friction: 5.0,
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });
        steeringLeverID = Entities.addEntity({
            name: "buggy steering lever",
            type: "Box",
            color: { blue: 40, green: 200, red: 20 },
            dimensions: { x: 0.02, y: steeringLeverLength, z: 0.02 },
            position: Vec3.sum(pos, steeringLeverOffset),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            script: Script.resolvePath("steeringLever.js"),
            userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
        });


        wheel0Constraint = Entities.addAction("hinge", wheel0ID, {
            pivot: { x: 0, y: 0, z: 0 },
            axis: { x: 1, y: 0, z: 0 },
            otherEntityID: carBodyID,
            otherPivot: wheel0Offset,
            otherAxis: { x: -1, y: 0, z: 0 },
            tag: "wheel 0"
        });
        wheel1Constraint = Entities.addAction("hinge", wheel1ID, {
            pivot: { x: 0, y: 0, z: 0 },
            axis: { x: 1, y: 0, z: 0 },
            otherEntityID: carBodyID,
            otherPivot: wheel1Offset,
            otherAxis: { x: -1, y: 0, z: 0 },
            tag: "wheel 1"
        });

        // wheel0Constraint = Entities.addAction("hinge", carBodyID, {
        //     pivot: wheel0Offset,
        //     axis: { x: 1, y: 0, z: 0 },
        //     otherEntityID: wheel0ID,
        //     otherPivot: { x: 0, y: 0, z: 0 },
        //     otherAxis: { x: 1, y: 0, z: 0 },
        //     tag: "wheel 0"
        // });
        // wheel1Constraint = Entities.addAction("hinge", carBodyID, {
        //     pivot: wheel1Offset,
        //     axis: { x: 1, y: 0, z: 0 },
        //     otherEntityID: wheel1ID,
        //     otherPivot: { x: 0, y: 0, z: 0 },
        //     otherAxis: { x: 1, y: 0, z: 0 },
        //     tag: "wheel 1"
        // });


        wheel2Constraint = Entities.addAction("hinge", wheel2ID, {
            pivot: { x: 0, y: 0, z: 0 },
            axis: { x: 1, y: 0, z: 0 },
            otherEntityID: carBodyID,
            otherPivot: wheel2Offset,
            otherAxis: { x: -1, y: 0, z: 0 },
            tag: "wheel 2"
        });
        wheel3Constraint = Entities.addAction("hinge", wheel3ID, {
            pivot: { x: 0, y: 0, z: 0 },
            axis: { x: 1, y: 0, z: 0 },
            otherEntityID: carBodyID,
            otherPivot: wheel3Offset,
            otherAxis: { x: -1, y: 0, z: 0 },
            tag: "wheel 3"
        });
        steeringLeverConstraint = Entities.addAction("hinge", steeringLeverID, {
            pivot: { x: 0, y: steeringLeverLength / -2.0, z: 0 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: carBodyID,
            otherPivot: Vec3.sum(steeringLeverOffset, { x: 0, y: -steeringLeverLength / 2.0, z: 0 }),
            otherAxis: { x: 0, y: 0, z: 1 },
            tag: "steering lever",
            softness: 0.0
        });

        Entities.editEntity(carBodyID, { gravity: { x: 0, y: -6, z: 0 } });
    }

    function deleteBuggy(event) {
        if (carBodyID) {
            Entities.deleteEntity(carBodyID);
            Entities.deleteEntity(wheel0ID);
            Entities.deleteEntity(wheel1ID);
            Entities.deleteEntity(wheel2ID);
            Entities.deleteEntity(wheel3ID);
            Entities.deleteEntity(steeringLeverID);
        }
        carBodyID = null;
        wheel0ID = null;
        wheel1ID = null;
        wheel2ID = null;
        wheel3ID = null;
        steeringLeverID = null;
    }

    function buggyForward(event) {
        print("FORWARD");
        if (!carBodyID) {
            return;
        }
        speed = event.speed;
        impulse = event.impulse;
        Entities.updateAction(wheel0ID, wheel0Constraint, { motorVelocity: speed, maxImpulse: impulse });
        Entities.updateAction(wheel1ID, wheel1Constraint, { motorVelocity: speed, maxImpulse: impulse });
        // Entities.updateAction(wheel2ID, wheel2Constraint, { motorVelocity: speed, maxImpulse: impulse });
        // Entities.updateAction(wheel3ID, wheel3Constraint, { motorVelocity: speed, maxImpulse: impulse });
    }

    function buggyStop(event) {
        print("STOP");
        if (!carBodyID) {
            return;
        }
        impulse = event.impulse;
        Entities.updateAction(wheel0ID, wheel0Constraint, { motorVelocity: 0.0, maxImpulse: impulse });
        Entities.updateAction(wheel1ID, wheel1Constraint, { motorVelocity: 0.0, maxImpulse: impulse });
        // Entities.updateAction(wheel2ID, wheel2Constraint, { motorVelocity: 0.0, maxImpulse: impulse });
        // Entities.updateAction(wheel3ID, wheel3Constraint, { motorVelocity: 0.0, maxImpulse: impulse });
    }

    function buggyBackward(event) {
        print("BACK");
        if (!carBodyID) {
            return;
        }
        speed = event.speed;
        impulse = event.impulse;
        Entities.updateAction(wheel0ID, wheel0Constraint, { motorVelocity: -speed, maxImpulse: impulse });
        Entities.updateAction(wheel1ID, wheel1Constraint, { motorVelocity: -speed, maxImpulse: impulse });
        // Entities.updateAction(wheel2ID, wheel2Constraint, { motorVelocity: -speed, maxImpulse: impulse });
        // Entities.updateAction(wheel3ID, wheel3Constraint, { motorVelocity: -speed, maxImpulse: impulse });
    }

    function onWebEventReceived(eventString) {
        print("received web event: " + JSON.stringify(eventString));
        if (typeof eventString === "string") {
            var event;
            try {
                event = JSON.parse(eventString);
            } catch(e) {
                return;
            }

            if (event["buggy-command"]) {
                var commandToFunctionMap = {
                    "new-buggy": newBuggy,
                    "delete-buggy": deleteBuggy,
                    "forward": buggyForward,
                    "stop": buggyStop,
                    "backward": buggyBackward,
                };

                var cmd = event["buggy-command"];
                if (commandToFunctionMap.hasOwnProperty(cmd)) {
                    var func = commandToFunctionMap[cmd];
                    func(event);
                }
            }
        }
    }

    var onBuggyScreen = false;
    var shouldActivateButton = false;

    function onClicked() {
        if (onBuggyScreen) {
            tablet.gotoHomeScreen();
        } else {
            shouldActivateButton = true;

            var defaultBuggySize = DEFAULT_BUGGY_SIZE;
            var defaultBuggySpeed = 4.0;
            var defaultBuggyImpulse = 4.0;

            tablet.gotoWebScreen(BUGGY_UI_URL +
                                 "?buggy-width=" + defaultBuggySize.x.toFixed(3).toString() +
                                 "&buggy-height=" + defaultBuggySize.y.toFixed(3).toString() +
                                 "&buggy-length=" + defaultBuggySize.z.toFixed(3).toString() +
                                 "&speed=" + defaultBuggySpeed.toFixed(3).toString() +
                                 "&impulse=" + defaultBuggyImpulse.toFixed(3).toString()
                                );
            onBuggyScreen = true;
        }
    }

    function onScreenChanged() {
        // for toolbar mode: change button to active when window is first openend, false otherwise.
        button.editProperties({isActive: shouldActivateButton});
        shouldActivateButton = false;
        onBuggyScreen = shouldActivateButton;
    }


    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }


    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);
    tablet.screenChanged.connect(onScreenChanged);
    Script.scriptEnding.connect(cleanup);

    Messages.subscribe('buggy');
    Messages.messageReceived.connect(function(channel, message, sender) {
        if (sender === MyAvatar.sessionUUID) {
            if (channel === 'buggy') {
                var data = JSON.parse(message);

                if (data.start) {
                    avatarLocalPosition = MyAvatar.localPosition;
                    avatarLocalRotation = MyAvatar.localOrientation;
                } else if (data.stop) {
                    avatarLocalPosition = null;
                    avatarLocalRotation = null;
                } else if (avatarLocalPosition && avatarLocalRotation) {
                    // MyAvatar.localPosition = avatarLocalPosition;
                    // MyAvatar.localOrientation = avatarLocalRotation;
                }

                var speed = data.speed;
                var direction = data.direction * -2.0; // radians

                Entities.updateAction(wheel0ID, wheel0Constraint, { motorVelocity: speed, maxImpulse: impulse });
                Entities.updateAction(wheel1ID, wheel1Constraint, { motorVelocity: speed, maxImpulse: impulse });
                Entities.updateAction(wheel2ID, wheel2Constraint, { motorVelocity: speed, maxImpulse: impulse });
                Entities.updateAction(wheel3ID, wheel3Constraint, { motorVelocity: speed, maxImpulse: impulse });

                Entities.editEntity(carBodyID, { angularVelocity: { x: 0, y: direction, z: 0} });

            }
        }
    });

}()); // END LOCAL_SCOPE
