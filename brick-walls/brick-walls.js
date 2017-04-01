
"use strict";

/* global Entities, Script, Tablet, MyAvatar, getEntityCustomData, setEntityCustomData, Vec3, Quat, Xform, Model, Assets */

(function() { // BEGIN LOCAL_SCOPE

    Script.include("/~/system/libraries/utils.js");
    Script.include("/~/system/libraries/Xform.js");

    var DEG_TO_RAD = Math.PI / 180.0;

    // var BRICK_WALLS_URL = "http://headache.hungry.com/~seth/hifi/brick-walls/brick-walls.html";
    var BRICK_WALLS_URL = Script.resolvePath("brick-walls.html");
    var BRICKS_RANGE = 20;

    var DEFAULT_BRICK_SIZE = { x: 0.2, y: 0.2, z: 0.4 };
    var DEFAULT_BRICKS_PER_ROW = 30;
    var DEFAULT_GAP = 0.03;
    var DEFAULT_BRICK_DATA = { index: 0 };

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: Script.resolvePath("brick-walls.svg"),
        text: "Bricks",
        sortOrder: 15
    });

    function getEntityName(entityID) {
        var props = Entities.getEntityProperties(entityID, ["name"]);
        return props.name;
    }

    function setBrickIndex(entityID, index) {
        setEntityCustomData("brick", entityID, { index: index });
    }

    function getBrickIndex(entityID) {
        var brickData = getEntityCustomData("brick", entityID, DEFAULT_BRICK_DATA);
        return brickData.index;
    }

    function isBrick(entityID) {
        return getEntityName(entityID) == "brick";
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
        return newBrickID;
    }

    function findRayIntersection(pickRay, includeNonBricks) {
        var toIgnore = [];
        while (true) {
            var rayPickResult = Entities.findRayIntersection(pickRay, true, [], toIgnore);
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


            print(JSON.stringify(rayPickResult));
            if (rayPickResult.intersects && rayPickResult.distance < brickLength + 0.01) {
                return false;
            }
        }
        return true;
    }

    function computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickDimensions, offsetAngle) {

        var lastBrickLength = lastBrickDimensions.z;
        var brickLength = params["brick-length"];
        var brickWidth = params["brick-width"];
        var brickHeight = params["brick-height"];

        var lastBrickEulers = Vec3.multiply(Quat.safeEulerAngles(lastBrickRotation), DEG_TO_RAD);
        var baseAngle = lastBrickEulers.y;

        var newBrickYAngle = baseAngle + offsetAngle;
        newBrickRotation = Quat.fromPitchYawRollRadians(0, newBrickYAngle, 0);

        var pickRayOrigin;
        var newBrickClosePosition;
        var newBrickPosition;
        var newBrickRotation;

        if (offsetAngle >= 0.0) {
            newBrickClosePosition = Vec3.sum(lastBrickPosition,
                                             Vec3.multiplyQbyV(lastBrickRotation,
                                                               { x: brickWidth / 2.0,
                                                                 y: 0,
                                                                 z: lastBrickLength / 2.0 + params.gap }));
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
                                                                 z: lastBrickLength / 2.0 + params.gap }));
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
        var newBrickClosePosition;

        // check the high limit
        var newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, highAngle);
        var pickRay = newBrickData.pickRay;
        var rayPickResult = findRayIntersection(pickRay, !force);
        if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
            highIntersects = true;
        }

        // check the low limit
        newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, lowAngle);
        pickRay = newBrickData.pickRay;
        rayPickResult = findRayIntersection(pickRay, !force);
        if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
            lowIntersects = true;
        }

        var hole = "";
        if (!lowIntersects && !highIntersects) {
            // holes on both sides, go straight
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
            // probably tracking the row below this... check the middle
            for (var angle = -90 * DEG_TO_RAD;
                 angle < 90 * DEG_TO_RAD;
                 angle += 5 * DEG_TO_RAD) {
                newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, angle);
                pickRay = newBrickData.pickRay;
                rayPickResult = findRayIntersection(pickRay, !force);
                if (rayPickResult.intersects && rayPickResult.distance <= brickHeight) {
                    middleIntersects = true;
                    if (angle < 0) {
                        highAngle = angle;
                        lowAngle = -90 * DEG_TO_RAD;
                        hole = "low";
                    } else {
                        lowAngle = angle;
                        highAngle = 90 * DEG_TO_RAD;
                        hole = "high";
                    }
                }
            }
            if (!force && !middleIntersects) {
                // don't put bricks over open air
                return null;
            }
        }

        while (true) {
            var middleAngle = (highAngle + lowAngle) / 2.0;
            newBrickData = computeNewBrick(params, lastBrickPosition, lastBrickRotation, lastBrickProps.dimensions, middleAngle);
            pickRay = newBrickData.pickRay;
            newBrickPosition = newBrickData.newBrickPosition;
            newBrickRotation = newBrickData.newBrickRotation;
            newBrickClosePosition = newBrickData.newBrickClosePosition;

            rayPickResult = findRayIntersection(pickRay, !force);

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
        //     position: newBrickClosePosition,
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
            print("... brick fits -- " + JSON.stringify(newBrickPosition));
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
            return newBrickID;
        } else if (canStartNewRow) {
            print("... starting new row");
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
            return uprowNewBrickID;
        }
        print("... brick doesn't fit");
        return null;
    }

    function addBrick(params, force) {
        var lastBrickID = findLastBrick();
        if (!lastBrickID) {
            print("...addFirstBrick");
            return addFirstBrick(params);
        } else {
            print("...addFollowingBrick after " + lastBrickID);
            return addFollowingBrick(lastBrickID, params, true, force);
        }
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
                        placedAny = true;
                    }
                } else {
                    break;
                }
            } else {
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
        // print("received web event: " + JSON.stringify(eventString));
        if (typeof eventString === "string") {
            var event;
            try {
                event = JSON.parse(eventString);
            } catch(e) {
                return;
            }

            if (event["brick-walls-command"]) {
                // converts strings to floats
                var params = {
                    "brick-width": parseFloat(event["brick-width"]),
                    "brick-height": parseFloat(event["brick-height"]),
                    "brick-length": parseFloat(event["brick-length"]),
                    "max-bricks-per-row": parseFloat(event["max-bricks-per-row"]),
                    "gap": parseFloat(event.gap)
                };

                if (event["brick-walls-command"] == "add-brick") {
                    addBrick(params, false);
                }
                if (event["brick-walls-command"] == "add-half-brick") {
                    params["brick-length"] /= 2.0;
                    addBrick(params, false);
                }
                if (event["brick-walls-command"] == "force-add-brick") {
                    addBrick(params, true);
                }
                if (event["brick-walls-command"] == "force-add-half-brick") {
                    params["brick-length"] /= 2.0;
                    addBrick(params, true);
                }
                if (event["brick-walls-command"] == "add-brick-row") {
                    addBrickRow(params);
                }
                if (event["brick-walls-command"] == "bake-brick-wall") {
                    bricksToOBJ(params);
                }
                if (event["brick-walls-command"] == "undo-one-brick") {
                    undoOneBrick(params);
                }
                if (event["brick-walls-command"] == "reset-bricks") {
                    resetBricks(params);
                }
                if (event["brick-walls-command"] == "reverse-wall-direction") {
                    reverseWallDirection(params, false);
                }
                if (event["brick-walls-command"] == "reverse-wall-direction-half") {
                    reverseWallDirection(params,  true);
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
            if (firstBrickID) {
                var props = Entities.getEntityProperties(firstBrickID, ["dimensions"]);
                if (props && props.dimensions) {
                    defaultBrickSize = props.dimensions;
                }
            }
            tablet.gotoWebScreen(BRICK_WALLS_URL +
                                 "?brick-width=" + defaultBrickSize.x +
                                 "&brick-height=" + defaultBrickSize.y +
                                 "&brick-length=" + defaultBrickSize.z +
                                 "&max-bricks-per-row=" + DEFAULT_BRICKS_PER_ROW +
                                 "&gap=" + DEFAULT_GAP
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
