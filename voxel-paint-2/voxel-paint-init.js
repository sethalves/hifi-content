
/* global Entities, Vec3, Script, acBaton, getEntityCustomData, setEntityCustomData */

(function () {
    Script.include("/~/system/libraries/utils.js");
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");


    this.batonName = null;
    this.baton = null;

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.readyBaton();
    };

    this.readyBaton = function () {
        if (!this.baton) {
            this.batonName = 'io.highfidelity.seth.voxel-paint:' + this.entityID;
            this.baton = acBaton({
                batonName: this.batonName,
                timeScale: 5000,
                serverTimeOut: 500 // ms
            });
        }
    };

    this.findEntityIDByName = function (entityName) {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 8);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == entityName) {
                return nearbyID;
            }
        }
        return null;
    };


    this.addPaintBrush = function (name, radius, color, colorIndex, position) {
        var brushID = Entities.addEntity({
            collidesWith: "",
            collisionMask: 0,
            color: { blue: 100, green: 100, red: 100 },
            dimensions: { x: 0.03, y: 0.3, z: 0.03 },
            name: name,
            position: position,
            rotation: {
                w: -0.69079118967056274,
                x: -0.59398794174194336,
                y: -0.29726099967956543,
                z: 0.28578627109527588
            },
            script: "http://headache.hungry.com/~seth/hifi/voxel-paint-2/voxel-paint-brush.js",
            shape: "Cube",
            type: "Box",
            userData: JSON.stringify({
                grabbableKey: {grabbable: true},
                wearable: {
                    joints: {LeftHand: [{x: -0.10801754891872406, y: 0.15447449684143066, z: 0.030637264251708984},
                                        {x: -0.32700979709625244, y: 0.623619794845581,
                                         z: 0.28943854570388794, w: 0.6483823657035828}],
                             RightHand: [{x: 0.11031082272529602, y: 0.19449540972709656, z: 0.0405043363571167},
                                         {x: 0.2807741165161133, y: 0.6332069635391235,
                                          z: 0.2997693121433258, w: -0.6557632088661194}]
                            }
                },
                color: colorIndex})
        });

        Entities.addEntity({
            collidesWith: "",
            collisionMask: 0,
            color: color,
            dimensions: { x: 2*radius, y: 2*radius, z: 2*radius },
            name: "voxel paint brush tip",
            parentID: brushID,
            localPosition: { x: 0, y: 0.16, z: 0 },
            type: "Sphere",
            userData: JSON.stringify({color: colorIndex})
        });

        return brushID;
    };

    this.addEraserBrush = function (name, radius, color, position) {
        var eraserID = Entities.addEntity({
            collidesWith: "",
            collisionMask: 0,
            color: { blue: 100, green: 100, red: 100 },
            dimensions: { x: 0.03, y: 0.3, z: 0.03 },
            name: name,
            position: position,
            rotation: {
                w: 0.60332643985748291,
                x: 0.62716102600097656,
                y: 0.44969868659973145,
                z: -0.20100706815719604
            },
            script: "http://headache.hungry.com/~seth/hifi/voxel-paint-2/voxel-paint-eraser.js",
            shape: "Cube",
            type: "Box",
            userData: JSON.stringify({
                grabbableKey: {grabbable: true},
                wearable: {
                    joints: {LeftHand: [{x: -0.10801754891872406, y: 0.15447449684143066, z: 0.030637264251708984},
                                        {x: -0.32700979709625244, y: 0.623619794845581,
                                         z: 0.28943854570388794, w: 0.6483823657035828}],
                             RightHand: [{x: 0.11031082272529602, y: 0.19449540972709656, z: 0.0405043363571167},
                                         {x: 0.2807741165161133, y: 0.6332069635391235,
                                          z: 0.2997693121433258, w: -0.6557632088661194}]
                            }
                }
            })
        });

        Entities.addEntity({
            collidesWith: "",
            collisionMask: 0,
            color: color,
            dimensions: { x: 2*radius, y: 2*radius, z: 2*radius },
            name: "voxel paint eraser tip",
            parentID: eraserID,
            localPosition: { x: 0, y: 0.16, z: 0 },
            type: "Sphere",
        });
        return eraserID;
    };


    this.addBrushes = function () {
        var position = Entities.getEntityProperties(this.entityID, ['position', 'dimensions']).position;

        var brush0Name = "voxel paint brush 0";
        var brush0ID = this.findEntityIDByName(brush0Name);
        if (!brush0ID) {
            var paintBrush0Position = Vec3.sum (position, { x: 0.4, y: 0, z: 0 });
            brush0ID = this.addPaintBrush(brush0Name, 0.08, {blue: 0, green: 255, red: 0}, 0, paintBrush0Position);
        }

        var brush1Name = "voxel paint brush 1";
        var brush1ID = this.findEntityIDByName(brush1Name);
        if (!brush1ID) {
            var paintBrush1Position = Vec3.sum (position, { x: 0.8, y: 0, z: 0 });
            brush1ID = this.addPaintBrush(brush1Name, 0.025, {blue: 0, green: 0, red: 255}, 1, paintBrush1Position);
        }

        var brush2Name = "voxel paint brush 2";
        var brush2ID = this.findEntityIDByName(brush2Name);
        if (!brush2ID) {
            var paintBrush2Position = Vec3.sum (position, { x: 1.2, y: 0, z: 0 });
            brush2ID = this.addPaintBrush(brush2Name, 0.08, {blue: 255, green: 0, red: 0}, 2, paintBrush2Position);
        }

        var eraser0Name = "voxel paint eraser 0";
        var eraserID = this.findEntityIDByName(eraser0Name);
        if (!eraserID) {
            var eraseBrushPosition = Vec3.sum (position, { x: -0.4, y: 0, z: 0 });
            eraserID = this.addEraserBrush(eraser0Name, 0.025, {blue: 0, green: 0, red: 0}, eraseBrushPosition);
        }
    };


    this.clearVoxelPaintSpace = function () {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 30);
        this.polyvoxes = {};
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == "voxel paint" || nearbyName == "voxel paint debug cube") {
                Entities.deleteEntity(nearbyID);
            }
        }
    };


    this.activateWithBaton = function () {
        var state = getEntityCustomData("state", this.entityID, "off");
        if (state == "off") {
            Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 255 }});
            state = "on";
            this.addBrushes();
            Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 0 }});
        } else { // if (state == "on") {
            Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 255 }});
            state = "off";
            this.clearVoxelPaintSpace();
            Entities.editEntity(this.entityID, { color: { blue: 0, green: 0, red: 255 }});
        }
        setEntityCustomData("state", this.entityID, state);
        this.baton.release();
    };

    this.activate = function () {
        var _this = this;
        this.readyBaton();
        this.baton.claim(
            function () { // onGrant
                _this.activateWithBaton();
            },
            function () { // onRelease
            },
            function () { // onDenied
            },
            function () { // onNoServerResponse
                _this.activateWithBaton();
            });
    };

    this.startNearTrigger = function (entityID) {
        this.activate();
    };

    this.stopNearTrigger = function (entityID) {
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        this.activate();
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
    };
});
