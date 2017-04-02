
"use strict";

/* global Entities, Script, Tablet, MyAvatar, getEntityCustomData, setEntityCustomData, Vec3, Quat, Xform, Model, Assets,
   Overlays*/

(function() { // BEGIN LOCAL_SCOPE

    Script.include("/~/system/libraries/utils.js");
    Script.include("/~/system/libraries/Xform.js");

    var DEG_TO_RAD = Math.PI / 180.0;
    // var RAD_TO_DEG = 180.0 / Math.PI;

    // var BRICK_WALLS_URL = "http://headache.hungry.com/~seth/hifi/brick-walls/brick-walls.html";
    var BRICK_WALLS_URL = Script.resolvePath("brick-walls.html");
    var BRICKS_RANGE = 20;

    var DEFAULT_BRICK_SIZE = { x: 0.2, y: 0.2, z: 0.4 };
    var DEFAULT_BRICKS_PER_ROW = 30;
    var DEFAULT_GAP = 0.03;
    var DEFAULT_BRICK_DATA = { index: 0, gap: 0.02 };

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: Script.resolvePath("brick-walls.svg"),
        text: "Bricks",
        sortOrder: 15
    });

    var getEntityNameCache = {};
    function getEntityName(entityID) {
        if (!getEntityNameCache.hasOwnProperty(entityID)) {
            getEntityNameCache[entityID] = Entities.getEntityProperties(entityID, ["name"]).name;
        }
        return getEntityNameCache[entityID];
    }

    var getBrickIndexCache = {};
    function setBrickIndex(entityID, index) {
        getBrickIndexCache[entityID] = index;
        var brickData = getEntityCustomData("brick", entityID, DEFAULT_BRICK_DATA);
        brickData.index = index;
        setEntityCustomData("brick", entityID, brickData);
    }

    function getBrickIndex(entityID) {
        if (!getBrickIndexCache.hasOwnProperty(entityID)) {
            var brickData = getEntityCustomData("brick", entityID, DEFAULT_BRICK_DATA);
            getBrickIndexCache[entityID] = brickData.index;
        }
        return getBrickIndexCache[entityID];
    }

    function setBrickNotes(entityID, gap) {
        var brickData = getEntityCustomData("brick", entityID, DEFAULT_BRICK_DATA);
        var notes = brickData.notes;
        if (!notes) {
            notes = {};
        }
        notes.gap = gap;
        brickData.notes = notes;
        setEntityCustomData("brick", entityID, brickData);
    }

    function getBrickNotes(entityID) {
        var brickData = getEntityCustomData("brick", entityID, DEFAULT_BRICK_DATA);
        if (brickData) {
            return brickData.notes;
        }
        return null;
    }

    var isBrickCache = {};
    function isBrick(entityID) {
        if (!isBrickCache.hasOwnProperty(entityID)) {
            isBrickCache[entityID] = (getEntityName(entityID) == "brick");
        }
        return isBrickCache[entityID];
    }

    function findFirstBrick() {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        var firstBrickID = null;
        var lowestIndex = -1;
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            if (getEntityName(entityID) == "brick") {
                var brickIndex = getBrickIndex(entityID);
                if (lowestIndex < 0 || brickIndex < lowestIndex) {
                    lowestIndex = brickIndex;
                    firstBrickID = entityID;
                }
            }
        }
        return firstBrickID;
    }

    function findLastBrick() {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        var lastBrickID = null;
        var highestIndex = -1;
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            if (getEntityName(entityID) == "brick") {
                var brickIndex = getBrickIndex(entityID);
                if (brickIndex > highestIndex) {
                    highestIndex = brickIndex;
                    lastBrickID = entityID;
                }
            }
        }
        return lastBrickID;
    }

    function addFirstBrick(params) {
        var position = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0, z: -1}));

        var brickDimensions = {
            x: params["brick-width"],
            y: params["brick-height"],
            z: params["brick-length"]
        };

        var newBrickID = Entities.addEntity({
            name: "brick",
            type: "Box",
            color: { blue: 128, green: 128, red: 128 },
            dimensions: brickDimensions,
            position: position,
            rotation: Quat.multiply(MyAvatar.orientation, Quat.fromPitchYawRollDegrees(0, 180, 0)),
            dynamic: true,
            collisionless: false,
            gravity: { x: 0, y: -1, z: 0 }
        });

        setBrickIndex(newBrickID, 0);
        setBrickNotes(newBrickID, params.gap);
        return newBrickID;
    }

    function findRayIntersection(pickRay, includeNonBricks) {
        var toIgnore = [];
        while (true) {
            var rayPickResult = Entities.findRayIntersection(pickRay, true, [], toIgnore);
            if (rayPickResult.intersects && getEntityName(rayPickResult.entityID) == "brick debug") {
                toIgnore.push(rayPickResult.entityID);
                continue;
            }
            if (includeNonBricks) {
                return rayPickResult;
            }
            if (rayPickResult.intersects) {
                if (isBrick(rayPickResult.entityID)) {
                    return rayPickResult;
                }
                toIgnore.push(rayPickResult.entityID);
            } else {
                return rayPickResult;
            }
        }
    }

    function brickFits(newBrickPosition, newBrickRotation, brickDimensions) {
        var trans = new Xform(newBrickRotation, newBrickPosition);
        // var brickWidth = brickDimensions.x;
        // var brickHeight = brickDimensions.y;
        var brickLength = brickDimensions.z;
        var toCheck = [{ x: 0, y: 0, z: -brickLength/2.0 }];
        for (var i = 0; i < toCheck.length; i++) {
            var pickRay = {
                origin: trans.xformPoint(toCheck[i]),
                direction: trans.xformVector({ x: 0, y: 0, z: 1 }),
                length: brickLength
            };

            // print("pickRay=" + JSON.stringify(pickRay));
            // print("pickRay.direction="+JSON.stringify(pickRay.direction));
            // print("pickRay.length="+JSON.stringify(pickRay.length));

            var rayPickResult = findRayIntersection(pickRay, true);

            // Entities.addEntity({
            //     name: "brick debug",
            //     type: "Sphere",
            //     color: { blue: 0, green: 0, red: 255 },
            //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
            //     position: pickRay.origin,
            //     dynamic: false,
            //     collisionless: true,
            //     lifetime: 60
            // });
            // Entities.addEntity({
            //     name: "brick debug",
            //     type: "Sphere",
            //     color: { blue: 0, green: 255, red: 0 },
            //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
            //     position: Vec3.sum(pickRay.origin, Vec3.multiply(pickRay.direction, pickRay.length)),
            //     dynamic: false,
            //     collisionless: true,
            //     lifetime: 60
            // });

            if (rayPickResult.intersects && rayPickResult.distance < brickLength + 0.01) {
                return false;
            }
        }
        return true;
    }

    function computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickDimensions, offsetAngle, hole) {

        var lastBrickWidth = lastBrickDimensions.x;
        var lastBrickLength = lastBrickDimensions.z;
        var brickLength = params["brick-length"];
        var brickWidth = params["brick-width"];
        var brickHeight = params["brick-height"];

        var lastBrickEulers = Vec3.multiply(Quat.safeEulerAngles(lastBrickRotation), DEG_TO_RAD);
        var baseAngle = lastBrickEulers.y;

        var newBrickYAngle = baseAngle + offsetAngle;
        newBrickRotation = Quat.fromPitchYawRollRadians(0, newBrickYAngle, 0);

        var pickRayOrigin;
        var newBrickCorner;
        var newBrickPosition;
        var newBrickRotation;

        if (offsetAngle >= 0.0) {
            newBrickCorner = Vec3.sum(lastBrickPosition,
                                             Vec3.multiplyQbyV(lastBrickRotation,
                                                               { x: lastBrickWidth / 2.0,
                                                                 y: 0,
                                                                 z: lastBrickLength / 2.0 + params.gap }));
            newBrickPosition = Vec3.sum(newBrickCorner,
                                        Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / -2.0,
                                                                              y: 0,
                                                                              z: brickLength / 2.0 }));

        } else {
            newBrickCorner = Vec3.sum(lastBrickPosition,
                                             Vec3.multiplyQbyV(lastBrickRotation,
                                                               { x: lastBrickWidth / -2.0,
                                                                 y: 0,
                                                                 z: lastBrickLength / 2.0 + params.gap }));
            newBrickPosition = Vec3.sum(newBrickCorner,
                                        Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / 2.0,
                                                                              y: 0,
                                                                              z: brickLength / 2.0 }));

        }

        if (hole == "high") {
            pickRayOrigin = Vec3.sum(newBrickPosition,
                                     Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / 2.0,
                                                                           y: brickHeight / 2.0 - 0.05,
                                                                           z: brickLength / 2.0 }));
        } else if (hole == "low") {
            pickRayOrigin = Vec3.sum(newBrickPosition,
                                     Vec3.multiplyQbyV(newBrickRotation, { x: brickWidth / -2.0,
                                                                           y: brickHeight / 2.0 - 0.05,
                                                                           z: brickLength / 2.0 }));

        } else {
            pickRayOrigin = Vec3.sum(newBrickPosition,
                                     Vec3.multiplyQbyV(newBrickRotation, { x: 0.0,
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
            newBrickCorner: newBrickCorner
        };
    }

    var firstInRowIDs = [];
    function addFollowingBrick(lastBrickID, params, canStartNewRow, force) {
        var lastBrickProps = Entities.getEntityProperties(lastBrickID, ["position", "rotation", "dimensions"]);

        var brickHeight = lastBrickProps.dimensions.y;
        var brickLength = lastBrickProps.dimensions.z;

        var lastBrickPosition = lastBrickProps.position;
        var lastBrickRotation = lastBrickProps.rotation;

        var highAngle = 90 * DEG_TO_RAD;
        var lowAngle = -90 * DEG_TO_RAD;

        var lowIntersects = false;
        var highIntersects = false;
        var middleIntersects = false;

        var newBrickPosition;
        var newBrickRotation;
        var newBrickCorner;

        // check the high limit
        var newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation,
                                           lastBrickProps.dimensions, highAngle, "high");
        var pickRay = newBrickData.pickRay;
        var rayPickResult = findRayIntersection(pickRay, !force);
        if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
            highIntersects = true;
        }

        // check the low limit
        newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation,
                                       lastBrickProps.dimensions, lowAngle, "low");
        pickRay = newBrickData.pickRay;
        rayPickResult = findRayIntersection(pickRay, !force);
        if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
            lowIntersects = true;
        }

        var hole = "";
        if (!lowIntersects && !highIntersects) {
            // holes on both sides, check the middle (below)
            highAngle = 0.0;
            lowAngle = 0.0;
            hole = "both";
        }
        if (!lowIntersects && highIntersects) {
            hole = "low";
        }
        if (lowIntersects && !highIntersects) {
            hole = "high";
        }
        if (lowIntersects && highIntersects) {
            // no hole on either side, go straight
            highAngle = 0.0;
            lowAngle = 0.0;
            hole = "neither";
        }

        if (hole == "both") {
            var searchStep = 10 * DEG_TO_RAD;
            // probably tracking the row below this... check the middle
            var angle;
            for (angle = -90 * DEG_TO_RAD;
                 angle < 90 * DEG_TO_RAD;
                 angle += searchStep) {
                newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation,
                                               lastBrickProps.dimensions, angle, "low");
                pickRay = newBrickData.pickRay;
                rayPickResult = findRayIntersection(pickRay, !force);
                if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
                    middleIntersects = true;
                    hole = "low";
                    lowAngle = angle - searchStep;
                }
            }
            for (angle = 90 * DEG_TO_RAD;
                 angle > -90 * DEG_TO_RAD;
                 angle -= searchStep) {
                newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation,
                                               lastBrickProps.dimensions, angle, "high");
                pickRay = newBrickData.pickRay;
                rayPickResult = findRayIntersection(pickRay, !force);
                if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
                    middleIntersects = true;
                    hole = "high";
                    highAngle = angle + searchStep;
                }
            }
            if (!force && !middleIntersects) {
                // don't put bricks over open air
                return null;
            }
        }

        // var nth = 0;
        // print("--- searching...");

        while (true) {
            var middleAngle = (highAngle + lowAngle) / 2.0;

            newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation,
                                           lastBrickProps.dimensions, middleAngle, hole);
            pickRay = newBrickData.pickRay;
            newBrickPosition = newBrickData.newBrickPosition;
            newBrickRotation = newBrickData.newBrickRotation;
            newBrickCorner = newBrickData.newBrickCorner;

            rayPickResult = findRayIntersection(pickRay, !force);

            // Entities.addEntity({
            //     name: "brick debug",
            //     type: "Sphere",
            //     color: { blue: 0, green: (255 / 10) * (nth), red: (255 / 10) * (10 - nth) },
            //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
            //     position: pickRay.origin,
            //     dynamic: false,
            //     collisionless: true,
            //     lifetime: 90
            // });
            // Entities.addEntity({
            //     name: "brick debug",
            //     type: "Sphere",
            //     color: { blue: 0, green: (255 / 10) * (nth), red: (255 / 10) * (10 - nth) },
            //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
            //     position: pickRay.origin,
            //     dynamic: false,
            //     collisionless: true,
            //     lifetime: 90
            // });
            // nth += 1;
            // Overlays.addOverlay("line3d", {
            //     color: {
            //         red: 200,
            //         green: 200,
            //         blue: 200
            //     },
            //     alpha: 1,
            //     visible: true,
            //     lineWidth: 2,
            //     start: pickRay.origin,
            //     end: Vec3.sum(pickRay.origin, Vec3.multiply(pickRay.direction, pickRay.length))
            // });


            // print("searching," +
            //       " low=" + (lowAngle * RAD_TO_DEG) +
            //       " middle=" + (middleAngle * RAD_TO_DEG) +
            //       " high=" + (highAngle * RAD_TO_DEG) +
            //       " hole=" + hole +
            //       " hit=" + (rayPickResult.intersects && rayPickResult.distance <= brickHeight));

            if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
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

            if (highAngle - lowAngle < (2 * DEG_TO_RAD)) {
                break;
            }
        }

        // Entities.addEntity({
        //     name: "brick debug",
        //     type: "Sphere",
        //     color: { blue: 128, green: 0, red: 0 },
        //     dimensions: {x: 0.05, y: 0.05, z: 0.05},
        //     position: newBrickCorner,
        //     dynamic: false,
        //     collisionless: true,
        //     lifetime: 60
        // });

        var brickDimensions = {
            x: params["brick-width"],
            y: params["brick-height"],
            z: params["brick-length"]
        };

        if (force || brickFits(newBrickPosition, newBrickRotation, brickDimensions)) {
            var newBrickID = Entities.addEntity({
                name: "brick",
                type: "Box",
                color: { blue: 128, green: 128, red: 128 },
                dimensions: brickDimensions,
                position: newBrickPosition,
                rotation: newBrickRotation,
                dynamic: false,
                collisionless: true
            });

            setBrickIndex(newBrickID, getBrickIndex(lastBrickID) + 1);
            setBrickNotes(newBrickID, params.gap);
            return newBrickID;
        } else if (canStartNewRow) {
            var trans = new Xform(newBrickRotation, newBrickPosition);
            var uprowNewBrickID = Entities.addEntity({
                name: "brick",
                type: "Box",
                color: { blue: 128, green: 128, red: 128 },
                dimensions: brickDimensions,
                position: Vec3.sum(newBrickPosition, trans.xformVector({ x: 0, y: brickHeight, z: brickLength / 2.0 })),
                rotation: newBrickRotation,
                dynamic: false,
                collisionless: true
            });

            setBrickIndex(uprowNewBrickID, getBrickIndex(lastBrickID) + 1);
            setBrickNotes(uprowNewBrickID, params.gap);
            firstInRowIDs.push(getBrickIndex(uprowNewBrickID));
            return uprowNewBrickID;
        }
        return null;
    }

    function addBrick(params, force) {
        var lastBrickID = findLastBrick();
        if (!lastBrickID) {
            return addFirstBrick(params);
        } else {
            return addFollowingBrick(lastBrickID, params, true, force);
        }
    }

    function addHalfBrick(params, force) {
        params["brick-length"] /= 2.0;
        addBrick(params, force);
        params["brick-length"] *= 2.0;
    }

    function forceAddBrick(params) {
        addBrick(params, true);
    }

    function forceAddHalfBrick(params) {
        addHalfBrick(params, true);
    }

    function addBrickRow(params) {
        var maxBricksPerRow = params["max-bricks-per-row"];
        var placedAny = false;

        for (var i = 0; i < maxBricksPerRow - 1; i++) {
            var lastBrickID = findLastBrick();
            if (!lastBrickID) {
                print("can't add row of bricks without at least one brick already placed");
                break;
            }

            var newBrickID = addFollowingBrick(lastBrickID, params, false);
            if (!newBrickID) {
                if (!placedAny) {
                    newBrickID = addFollowingBrick(lastBrickID, params, true);
                    if (newBrickID) {
                        if (!placedAny) {
                            firstInRowIDs.push(getBrickIndex(newBrickID));
                        }
                        placedAny = true;
                    }
                } else {
                    break;
                }
            } else {
                if (!placedAny) {
                    firstInRowIDs.push(getBrickIndex(newBrickID));
                }
                placedAny = true;
            }
        }
    }

    function reverseWallDirection(params, half) {
        var lastBrickID = findLastBrick();
        var lastBrickProps = Entities.getEntityProperties(lastBrickID, ["position", "rotation", "dimensions"]);
        var lastBrickTrans = new Xform(lastBrickProps.rotation, lastBrickProps.position);

        var brickDimensions = {
            x: params["brick-width"],
            y: params["brick-height"],
            z: params["brick-length"]
        };
        if (half) {
            brickDimensions.z /= 2.0;
        }
        var newBrickPosition = Vec3.sum(lastBrickProps.position, lastBrickTrans.xformVector({
            x: 0,
            y: lastBrickProps.dimensions.y / 2.0 + brickDimensions.y / 2.0,
            z: lastBrickProps.dimensions.z / 2.0 - brickDimensions.z / 2.0
        }));
        var newBrickRotation = Quat.multiply(lastBrickProps.rotation, Quat.fromVec3Degrees({ x: 0, y: 180, z: 0}));

        var newBrickID = Entities.addEntity({
            name: "brick",
            type: "Box",
            color: { blue: 128, green: 128, red: 128 },
            dimensions: brickDimensions,
            position: newBrickPosition,
            rotation: newBrickRotation,
            dynamic: false,
            collisionless: true
        });

        setBrickIndex(newBrickID, getBrickIndex(lastBrickID) + 1);
        setBrickNotes(newBrickID, params.gap);
    }

    function resetBricks(params) {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        var firstBrickID = findFirstBrick();
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            if (entityID == firstBrickID) {
                continue;
            }
            if (isBrick(entityID)) {
                Entities.deleteEntity(entityID);
            }
        }
    }

    function clearBricks(params) {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            if (isBrick(entityID)) {
                Entities.deleteEntity(entityID);
            }
        }
    }

    function undoOneBrick(params) {
        var firstBrickID = findFirstBrick();
        var lastBrickID = findLastBrick();
        if (firstBrickID != lastBrickID) {
            Entities.deleteEntity(lastBrickID);
        }
    }

    function undoRow(params) {
        print("undoRow -- firstInRowIDs = " + JSON.stringify(firstInRowIDs));
        if (firstInRowIDs.length > 0) {
            var firstBrickID = findFirstBrick();
            var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
            var rowStart = firstInRowIDs.pop();
            for (var i = 0; i < allEntities.length; i++) {
                var entityID = allEntities[i];
                if (isBrick(entityID) &&
                    entityID != firstBrickID &&
                    getBrickIndex(entityID) >= rowStart) {
                    Entities.deleteEntity(entityID);
                }
            }
        }
    }

    function brickToMesh(entityID) {
        var props = Entities.getEntityProperties(entityID, ["position", "rotation", "dimensions"]);
        var trans = new Xform(props.rotation, props.position);

        var hw = props.dimensions.x / 2.0;
        var hh = props.dimensions.y / 2.0;
        var hl = props.dimensions.z / 2.0;
        var vertices = [{ x: -hw, y: -hh, z: hl }, // 3
                        { x: -hw, y: -hh, z: -hl }, // 4
                        { x: hw, y: -hh, z: -hl }, // 1

                        { x: hw, y: -hh, z: -hl }, // 1
                        { x: hw, y: -hh, z: hl }, // 2
                        { x: -hw, y: -hh, z: hl }, // 3

                        { x: hw, y: hh, z: hl }, // 6
                        { x: -hw, y: hh, z: hl }, // 7
                        { x: -hw, y: -hh, z: hl }, // 3

                        { x: hw, y: hh, z: -hl }, // 5
                        { x: hw, y: hh, z: hl }, // 6
                        { x: hw, y: -hh, z: hl }, // 2

                        { x: hw, y: hh, z: -hl }, // 5
                        { x: hw, y: -hh, z: hl }, // 2
                        { x: hw, y: -hh, z: -hl }, // 1

                        { x: -hw, y: hh, z: hl }, // 7
                        { x: -hw, y: -hh, z: -hl }, // 4
                        { x: -hw, y: -hh, z: hl }, // 3

                        { x: -hw, y: -hh, z: -hl }, // 4
                        { x: -hw, y: hh, z: hl }, // 7
                        { x: -hw, y: hh, z: -hl }, // 8

                        { x: -hw, y: -hh, z: -hl }, // 4
                        { x: -hw, y: hh, z: -hl }, // 8
                        { x: hw, y: -hh, z: -hl }, // 1

                        { x: hw, y: -hh, z: hl }, // 2
                        { x: hw, y: hh, z: hl }, // 6
                        { x: -hw, y: -hh, z: hl }, // 3

                        { x: -hw, y: hh, z: -hl }, // 8
                        { x: hw, y: hh, z: -hl }, // 5
                        { x: hw, y: -hh, z: -hl }, // 1

                        { x: -hw, y: hh, z: -hl }, // 8
                        { x: -hw, y: hh, z: hl }, // 7
                        { x: hw, y: hh, z: -hl }, // 5

                        { x: -hw, y: hh, z: hl }, // 7
                        { x: hw, y: hh, z: hl }, // 6
                        { x: hw, y: hh, z: -hl } // 5
                       ].map(function(inModelFramePoint) {
                           return trans.xformPoint(inModelFramePoint);
                       });

        var normals = [ { x: 0, y: -1, z: 0 },
                        { x: 0, y: -1, z: 0 },
                        { x: 0, y: -1, z: 0 },
                        { x: 0, y: -1, z: 0 },
                        { x: 0, y: -1, z: 0 },
                        { x: 0, y: -1, z: 0 },
                        { x: -0, y: 0, z: 1 },
                        { x: -0, y: 0, z: 1 },
                        { x: -0, y: 0, z: 1 },
                        { x: 1, y: 0, z: 0 },
                        { x: 1, y: 0, z: 0 },
                        { x: 1, y: 0, z: 0 },
                        { x: 1, y: 0, z: 0 },
                        { x: 1, y: 0, z: 0 },
                        { x: 1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: -1, y: 0, z: 0 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: -0, z: 1 },
                        { x: 0, y: -0, z: 1 },
                        { x: 0, y: -0, z: 1 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: 0, z: -1 },
                        { x: 0, y: 1, z: 0 },
                        { x: 0, y: 1, z: 0 },
                        { x: 0, y: 1, z: 0 },
                        { x: 0, y: 1, z: 0 },
                        { x: 0, y: 1, z: 0 },
                        { x: 0, y: 1, z: 0 }
                      ].map(function(inModelFrameNormal) {
                          return trans.xformVector(inModelFrameNormal);
                      });

        var faces = [{ vertices: [0, 1, 2] },
                     { vertices: [3, 4, 5] },
                     { vertices: [6, 7, 8] },
                     { vertices: [9, 10, 11] },
                     { vertices: [12, 13, 14] },
                     { vertices: [15, 16, 17] },
                     { vertices: [18, 19, 20] },
                     { vertices: [21, 22, 23] },
                     { vertices: [24, 25, 26] },
                     { vertices: [27, 28, 29] },
                     { vertices: [30, 31, 32] },
                     { vertices: [33, 34, 35] }];

        return {
            mesh: Model.newMesh(vertices, normals, faces),
            vertices: vertices
        };
    }

    function keepMin(low, v) {
        if (!low.x || v.x < low.x) {
            low.x = v.x;
        }
        if (!low.y || v.y < low.y) {
            low.y = v.y;
        }
        if (!low.z || v.z < low.z) {
            low.z = v.z;
        }
    }

    function keepMax(high, v) {
        if (!high.x || v.x > high.x) {
            high.x = v.x;
        }
        if (!high.y || v.y > high.y) {
            high.y = v.y;
        }
        if (!high.z || v.z > high.z) {
            high.z = v.z;
        }
    }

    function bricksToOBJ(params) {
        var allEntities = Entities.findEntities(MyAvatar.position, BRICKS_RANGE);
        var meshes = [];

        var low = { x: false, y: false, z: false };
        var high = { x: false, y: false, z: false };
        var foreachVertexFunction = function (vertex) {
            keepMin(low, vertex);
            keepMax(high, vertex);
        };

        for (var i = 0; i < allEntities.length; i++) {
            var entityID = allEntities[i];
            if (!isBrick(entityID)) {
                continue;
            }
            var brickToMeshResult = brickToMesh(entityID);
            meshes.push(brickToMeshResult.mesh);
            brickToMeshResult.vertices.forEach(foreachVertexFunction);
        }
        var objData = Model.meshToOBJ(meshes);
        var nth = Math.floor((Math.random() * 1000) + 1);
        var fileName = "/brick-walls-" + nth + ".obj";
        var position = Vec3.multiply(Vec3.sum(low, high), 0.5);

        clearBricks(params);

        Assets.uploadData(objData, function(url, hash) {
            Assets.setMapping(fileName, hash, function() {
                Entities.addEntity({
                    type: "Model",
                    modelURL: "atp:" + fileName,
                    position: Vec3.sum(position, { x: 0, y: 0, z: 0 }),
                    name: "brick-wall",
                    dynamic: false,
                    collisionless: true
                });
            });
        });
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

            if (event["brick-walls-command"]) {
                var commandToFunctionMap = {
                    "add-brick": addBrick,
                    "add-half-brick": addHalfBrick,
                    "force-add-brick": forceAddBrick,
                    "force-add-half-brick": forceAddHalfBrick,
                    "add-brick-row": addBrickRow,
                    "bake-brick-wall": bricksToOBJ,
                    "undo-one-brick": undoOneBrick,
                    "undo-row": undoRow,
                    "reset-bricks": resetBricks,
                    "reverse-wall-direction": reverseWallDirection,
                    "reverse-wall-direction-half": reverseWallDirection
                };

                var cmd = event["brick-walls-command"];
                if (commandToFunctionMap.hasOwnProperty(cmd)) {
                    var func = commandToFunctionMap[cmd];
                    func(event);
                }
            }
        }
    }

    var onBricksScreen = false;
    var shouldActivateButton = false;

    function onClicked() {
        if (onBricksScreen) {
            tablet.gotoHomeScreen();
        } else {
            shouldActivateButton = true;

            var firstBrickID = findFirstBrick();
            var defaultBrickSize = DEFAULT_BRICK_SIZE;
            var defaultGap = DEFAULT_GAP;
            if (firstBrickID) {
                var props = Entities.getEntityProperties(firstBrickID, ["dimensions"]);
                if (props && props.dimensions) {
                    defaultBrickSize = props.dimensions;
                }
            }

            var lastBrickID = findLastBrick();
            if (lastBrickID) {
                var notes = getBrickNotes(lastBrickID);
                if (notes) {
                    defaultGap = notes.gap;
                }
            }

            tablet.gotoWebScreen(BRICK_WALLS_URL +
                                 "?brick-width=" + defaultBrickSize.x.toFixed(3).toString() +
                                 "&brick-height=" + defaultBrickSize.y.toFixed(3).toString() +
                                 "&brick-length=" + defaultBrickSize.z.toFixed(3).toString() +
                                 "&max-bricks-per-row=" + DEFAULT_BRICKS_PER_ROW.toString() +
                                 "&gap=" + defaultGap.toFixed(3).toString()
                                );
            onBricksScreen = true;
        }
    }

    function onScreenChanged() {
        // for toolbar mode: change button to active when window is first openend, false otherwise.
        button.editProperties({isActive: shouldActivateButton});
        shouldActivateButton = false;
        onBricksScreen = false;
    }

    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);
    tablet.screenChanged.connect(onScreenChanged);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
