
"use strict";

/* global Entities, Script, Tablet, MyAvatar, getEntityCustomData, setEntityCustomData, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    Script.include("/~/system/libraries/utils.js");

    var BRICK_WALLS_URL = "http://headache.hungry.com/~seth/hifi/brick-walls/brick-walls.html";
    var BRICKS_RANGE = 20;
    var brickSize = { x: 0.2, y: 0.2, z: 0.4 };
    var DEG_TO_RAD = Math.PI / 180.0;

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: "http://headache.hungry.com/~seth/hifi/brick-walls/brick-walls.svg",
        text: "Bricks",
        sortOrder: 15
    });

    function findBrick(brickType) {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            var props = Entities.getEntityProperties(entityID, ["name"]);
            if (props.name == "brick") {
                var brickData = getEntityCustomData("brick", entityID, {});
                if (brickData[brickType]) {
                    return entityID;
                }
            }
        }
        return null;
    }

    function getEntityName(entityID) {
        var props = Entities.getEntityProperties(entityID, ["name"]);
        return props.name;
    }

    function findFirstBrick() {
        return findBrick("first");
    }

    function findLastBrick() {
        return findBrick("last");
    }

    function setBrickAsFirst(entityID, value) {
        var brickData = getEntityCustomData("brick", entityID, {});
        brickData.first = value;
        setEntityCustomData("brick", entityID, brickData);
    }

    function setBrickAsLast(entityID, value) {
        var brickData = getEntityCustomData("brick", entityID, {});
        brickData.last = value;
        setEntityCustomData("brick", entityID, brickData);
    }

    function addFirstBrick() {
        var position = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0, z: -1}));

        var newBrickID = Entities.addEntity({
            name: "brick",
            type: "Box",
            color: { blue: 128, green: 128, red: 128 },
            dimensions: brickSize,
            position: position,
            rotation: Quat.multiply(MyAvatar.orientation, Quat.fromPitchYawRollDegrees(0, 180, 0)),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: -1, z: 0 }
        });

        setBrickAsFirst(newBrickID, true);
        setBrickAsLast(newBrickID, true);
    }

    function computeNewBrick(lastBrickPosition, lastBrickRotation, lastBrickDimensions, offsetAngle) {

        var brickLength = lastBrickDimensions.z;
        var brickWidth = lastBrickDimensions.x;
        var brickHeight = lastBrickDimensions.y;

        var lastBrickEulers = Vec3.multiply(Quat.safeEulerAngles(lastBrickRotation), DEG_TO_RAD);
        var baseAngle = lastBrickEulers.y;

        var newBrickYAngle = baseAngle + offsetAngle;
        newBrickRotation = Quat.fromPitchYawRollRadians(0, newBrickYAngle, 0);

        var gap = 0.0;

        var pickRayOrigin;
        var newBrickClosePosition;
        var newBrickPosition;
        var newBrickRotation;

        if (offsetAngle >= 0.0) {
            newBrickClosePosition = Vec3.sum(lastBrickPosition,
                                             Vec3.multiplyQbyV(lastBrickRotation,
                                                               { x: brickWidth / 2.0,
                                                                 y: 0,
                                                                 z: brickLength / 2.0 + gap }));
            newBrickPosition = Vec3.sum(newBrickClosePosition,
                                        Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / -2.0,
                                                                              y: 0,
                                                                              z: brickLength / 2.0 }));

            pickRayOrigin = Vec3.sum(newBrickPosition,
                                     Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / 2.0,
                                                                           y: brickHeight / 2.0 - 0.05,
                                                                           z: brickLength / 2.0 }));
        } else {
            newBrickClosePosition = Vec3.sum(lastBrickPosition,
                                             Vec3.multiplyQbyV(lastBrickRotation,
                                                               { x: brickWidth / -2.0,
                                                                 y: 0,
                                                                 z: brickLength / 2.0 + gap }));
            newBrickPosition = Vec3.sum(newBrickClosePosition,
                                        Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / 2.0,
                                                                              y: 0,
                                                                              z: brickLength / 2.0 }));

            pickRayOrigin = Vec3.sum(newBrickPosition,
                                     Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / -2.0,
                                                                           y: brickHeight / 2.0 - 0.05,
                                                                           z: brickLength / 2.0 }));

        }

        var pickRay = {
            origin: pickRayOrigin,
            direction: { x: 0, y: -1, z: 0 },
            length: brickHeight // divided by 2?
        };

        return {
            pickRay: pickRay,
            newBrickPosition: newBrickPosition,
            newBrickRotation: newBrickRotation,
            newBrickClosePosition: newBrickClosePosition
        };
    }

    function addFollowingBrick(lastBrickID) {
        var lastBrickProps = Entities.getEntityProperties(lastBrickID, ["position", "rotation", "dimensions"]);

        var brickHeight = lastBrickProps.dimensions.y;

        var lastBrickPosition = lastBrickProps.position;
        var lastBrickRotation = lastBrickProps.rotation;

        var highAngle = 90 * DEG_TO_RAD;
        var lowAngle = -90 * DEG_TO_RAD;

        var lowIntersects = false;
        var highIntersects = false;

        var newBrickPosition;
        var newBrickRotation;
        var newBrickClosePosition;

        // check the high limit
        var newBrickData = computeNewBrick(lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, highAngle);
        var pickRay = newBrickData.pickRay;
        var rayPickResult = Entities.findRayIntersection(pickRay, true);
        if (rayPickResult.intersects && rayPickResult.distance < brickHeight) {
            highIntersects = true;
        }

        // check the low limit
        newBrickData = computeNewBrick(lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, lowAngle);
        pickRay = newBrickData.pickRay;
        rayPickResult = Entities.findRayIntersection(pickRay, true);
        if (rayPickResult.intersects && rayPickResult.distance < brickHeight) {
            lowIntersects = true;
        }

        var hole;
        if (!lowIntersects && !highIntersects) {
            // holes on both sides, can't cope
            return false;
        }
        if (!lowIntersects && highIntersects) {
            hole = "low";
        }
        if (lowIntersects && !highIntersects) {
            hole = "high";
        }
        if (lowIntersects && highIntersects) {
            // no hole on either side, can't cope
            return false;
        }

        while (true) {
            if (highAngle - lowAngle < (2 * DEG_TO_RAD)) {
                break;
            }
            var middleAngle = (highAngle + lowAngle) / 2.0;
            newBrickData = computeNewBrick(lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, middleAngle);
            pickRay = newBrickData.pickRay;
            newBrickPosition = newBrickData.newBrickPosition;
            newBrickRotation = newBrickData.newBrickRotation;
            newBrickClosePosition = newBrickData.newBrickClosePosition;

            rayPickResult = Entities.findRayIntersection(pickRay, true);

            // Entities.addEntity({
            //     name: "brick debug",
            //     type: "Sphere",
            //     color: { blue: 0, green: 255, red: 0 },
            //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
            //     position: pickRay.origin,
            //     dynamic: false,
            //     collisionless: true,
            //     lifetime: 60
            // });

            if (rayPickResult.intersects && rayPickResult.distance < brickHeight) {
                if (hole == "high") {
                    lowAngle = middleAngle;
                } else {
                    highAngle = middleAngle;
                }
            } else {
                if (hole == "high") {
                    highAngle = middleAngle;
                } else {
                    lowAngle = middleAngle;
                }
            }
        }

        // Entities.addEntity({
        //     name: "brick debug",
        //     type: "Sphere",
        //     color: { blue: 128, green: 0, red: 0 },
        //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
        //     position: newBrickClosePosition,
        //     dynamic: false,
        //     collisionless: true,
        //     lifetime: 60
        // });

        var newBrickID = Entities.addEntity({
            name: "brick",
            type: "Box",
            color: { blue: 128, green: 128, red: 128 },
            dimensions: brickSize,
            position: newBrickPosition,
            rotation: newBrickRotation,
            dynamic: false,
            collisionless: true
        });

        setBrickAsLast(lastBrickID, false);
        setBrickAsLast(newBrickID, true);

        return newBrickID;
    }

    function addBrick() {
        var lastBrickID = findLastBrick();
        if (!lastBrickID) {
            addFirstBrick();
        } else {
            addFollowingBrick(lastBrickID);
        }
    }

    function resetBricks() {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            var props = Entities.getEntityProperties(entityID, ["name"]);
            if (props.name == "brick") {
                var brickData = getEntityCustomData("brick", entityID, {});
                if (!brickData.first) {
                    Entities.deleteEntity(entityID);
                } else {
                    setBrickAsLast(entityID, true);
                }
            }
        }
    }

    function onClicked() {
        tablet.gotoWebScreen(BRICK_WALLS_URL);
    }

    function onWebEventReceived(eventString) {
        print("received web event: " + JSON.stringify(eventString));
        var event;
        if (typeof eventString === "string") {
            try {
                event = JSON.parse(eventString);
            } catch(e) {
                return;
            }

            if (event["brick-walls-command"]) {
                if (event["brick-walls-command"] == "add-brick") {
                    print("add brick");
                    addBrick();
                }
                if (event["brick-walls-command"] == "reset-bricks") {
                    print("reset bricks");
                    resetBricks();
                }
            } else {
                print("no brick-walls-command");
            }
        } else {
            print("not string: " + typeof eventString);
        }
    }

    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);

    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
