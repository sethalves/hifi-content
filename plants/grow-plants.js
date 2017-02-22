"use strict";

/* global Entities, Script, Tablet, Vec3, Quat, getEntityCustomData, setEntityCustomData */

(function() { // BEGIN LOCAL_SCOPE

    Script.include("/~/system/libraries/utils.js");

    // var overlays = [];
    var GROW_RANGE = 100;
    var seedSize = 0.05;
    var NULL_UUID = "{00000000-0000-0000-0000-000000000000}";

    var initialPlantData = {
        age: 0.0,
        size: 0.1
    };

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: "http://headache.hungry.com/~seth/hifi/plants/plants.svg",
        text: "PLANTS",
        sortOrder: 15
    });


    function getDimensions(plantSize) {
        var leafHighpoint = 1.0;
        return {
            x: 2.0 * plantSize * Math.sin(Math.PI / 3.0),
            y: leafHighpoint * plantSize,
            z: plantSize + (plantSize * Math.sin(Math.PI / 6.0))
        };
    }


    function getLeafTip(pos, rot, plantSize, n) {
        var leafTip = { x: 0, y: 0.7 * plantSize, z: plantSize };
        var tipRotation = Quat.multiply(rot, Quat.fromVec3Degrees({ x: 0, y: 120 * n, z: 0 }));
        var dir = { x: 0, y: 0, z: 0.2 };
        var leafTipRotated = Vec3.multiplyQbyV(tipRotation, leafTip);
        var tip = Vec3.sum(pos, leafTipRotated);
        return {
            tip: tip,
            direction: Vec3.normalize(Vec3.multiplyQbyV(tipRotation, dir))
        };
    }

    function calculateZReg() {
        var a = 1 + Math.sin(Math.PI / 6.0);
        var b = 1 - (a / 2.0);
        return 0.5 - (b / a);
    }


    function fixUpPlant(plantID, plantSize) {
        var zReg = calculateZReg();
        Entities.editEntity(plantID, {
            registrationPoint: { x: 0.5, y: 0, z: zReg },
            dimensions: getDimensions(plantSize)
        });
    }

    function makeSeed(plantID, pos, rot) {
        var vel = {
            x: 3 * (Math.random() - 0.5),
            y: 1.0,
            z: 3 * (Math.random() - 0.5)
        };

        Entities.addEntity({
            name: "Seed-0",
            color: { blue: 30, green: 42, red: 200 },
            dimensions: { x: seedSize, y: seedSize, z: seedSize },
            position: Vec3.sum(pos, { x: 0, y: 0.5, z: 0 }),
            type: "Sphere",
            velocity: vel,
            lifetime: 120,
            gravity: { x: 0, y: -1, z: 0 },
            dynamic: true
        });
    }

    function getCanGrow(plantID, pos, rot, plantSize) {
        var canGrow = true;
        for (var i = 0; i < 3; i++) {
            var leafTip = getLeafTip(pos, rot, plantSize, i);
            var pickRay = {
                origin: leafTip.tip,
                direction: leafTip.direction
            };
            var intersection = Entities.findRayIntersection(pickRay, true, [], [plantID], true, false);
            // print(JSON.stringify(intersection));
            if (intersection.intersects && intersection.distance < 0.2) {
                canGrow = false;
                break;
            }
        }
        return canGrow;
    }

    function setPlantData(plantID, plantSize, plantAge) {
        setEntityCustomData("plant", plantID, {
            size: plantSize,
            age: plantAge + 1
        });
    }

    function agePlant(plantID, properties) {
        var plantData = getEntityCustomData('plant', plantID, initialPlantData);
        var plantSize = plantData.size;
        var plantAge = plantData.age;
        if (!plantSize) {
            plantSize = 0.1;
        }
        if (!plantAge) {
            plantAge = 0;
        }
        fixUpPlant(plantID, plantSize);
        var pos = properties.position;
        var rot = properties.rotation;

        // var line0start = pos;
        // var line0end = Vec3.sum(line0start, { x: 0, y: 1, z: 0 });
        // var line0ID = Overlays.addOverlay("line3d", { start: line0start, end: line0end });
        // overlays.push(line0ID);

        // for (var i = 0; i < 3; i++) {
        //     var leafTip = getLeafTip(pos, rot, plantSize, i);
        //     var lineTipID = Overlays.addOverlay("line3d", {
        //         start: leafTip.tip,
        //         end: Vec3.sum(leafTip.tip, Vec3.multiply(leafTip.direction, 0.2))
        //     });
        //     overlays.push(lineTipID);
        // }

        var canGrow = getCanGrow(plantID, pos, rot, plantSize);

        if (canGrow) {
            // grow plant
            plantSize += 0.1;
            fixUpPlant(plantID, plantSize);
        }
        setPlantData(plantID, plantSize, plantAge);

        if (Math.random() >= 0.9) {
            makeSeed(plantID, pos, rot);
        }

        if (plantAge >= 20) {
            Entities.deleteEntity(plantID);
        }
    }

    function growSeed(seedID, properties) {
        var plantSize = 0.1;
        var plantAge = 0;
        var pos = properties.position;
        pos = Vec3.subtract(pos, { x: 0, y: seedSize / 2.0, z: 0 }); // move down length of seed radius

        var rot = properties.rotation;
        var eulerRot = Quat.safeEulerAngles(rot);
        rot = Quat.fromVec3Radians({ x: 0, y: eulerRot.y, z: 0 });

        if (getCanGrow(NULL_UUID, pos, rot, plantSize)) {
            var zReg = calculateZReg();
            var plantID = Entities.addEntity({
                type: "Model",
                name: "Plant-0",
                modelURL: "http://headache.hungry.com/~seth/hifi/plants/plant-0.obj",
                position: pos,
                rotation: rot,
                registrationPoint: { x: 0.5, y: 0, z: zReg },
                dimensions: getDimensions(plantSize),
                dynamic: false,
                collisionless: true
            });
            setPlantData(plantID, plantSize, plantAge);
        }
        Entities.deleteEntity(seedID);
    }


    function onClicked() {
        var allEntities = Entities.findEntities({ x: 90, y: 0, z: 90 }, GROW_RANGE);
        allEntities.forEach(function(entityID) {
            var props = Entities.getEntityProperties(entityID, ["name", "modelURL", "position", "rotation", "dimensions"]);
            if (props.name == "Plant-0") {
                agePlant(entityID, props);
            }
            if (props.name == "Seed-0") {
                growSeed(entityID, props);
            }
            return false;
        });

        // Script.setTimeout(function() {
        //     overlays.forEach(function(overlayID) {
        //         Overlays.deleteOverlay(overlayID);
        //     });
        //     overlays = [];
        // }, 4000);
    }

    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
