
/* global Entities, Vec3, Script, acBaton, getEntityCustomData, setEntityCustomData */

(function () {
    Script.include("/~/system/libraries/utils.js");
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");


    this.batonName = null;
    this.baton = null;

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.slices = 5;
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

    this.addBrushes = function () {
        var position = Entities.getEntityProperties(this.entityID, ['position', 'dimensions']).position;
        var paintBrushPosition = Vec3.sum (position, { "x": 0.4, "y": 0, "z": 0 });
        var eraseBrushPosition = Vec3.sum (position, { "x": -0.4, "y": 0, "z": 0 });

        var brushID = this.findEntityIDByName("voxel paint brush");
        if (!brushID) {
            brushID = Entities.addEntity({
                "collidesWith": "",
                "collisionMask": 0,
                "color": { "blue": 0, "green": 0, "red": 255 },
                "dimensions": { "x": 0.03, "y": 0.3, "z": 0.03 },
                "name": "voxel paint brush",
                "position": paintBrushPosition,
                "rotation": {
                    "w": -0.69079118967056274,
                    "x": -0.59398794174194336,
                    "y": -0.29726099967956543,
                    "z": 0.28578627109527588
                },
                "script": "http://23.253.109.180/~seth/hifi/voxel-paint/voxel-paint-brush.js",
                "shape": "Cube",
                "type": "Box",
                "userData": "{\"grabbableKey\":{\"grabbable\":true},\"wearable\":{\"joints\":{\"LeftHand\":[{\"x\":-0.10801754891872406,\"y\":0.15447449684143066,\"z\":0.030637264251708984},{\"x\":-0.32700979709625244,\"y\":0.623619794845581,\"z\":0.28943854570388794,\"w\":0.6483823657035828}],\"RightHand\":[{\"x\":0.11031082272529602,\"y\":0.19449540972709656,\"z\":0.0405043363571167},{\"x\":0.2807741165161133,\"y\":0.6332069635391235,\"z\":0.2997693121433258,\"w\":-0.6557632088661194}]}}}"
            });

            Entities.addEntity({
                "collidesWith": "",
                "collisionMask": 0,
                "color": { "blue": 255, "green": 0, "red": 0 },
                "dimensions": { "x": 0.05, "y": 0.05, "z": 0.05 },
                "name": "voxel paint brush tip",
                "parentID": brushID,
                "localPosition": { "x": 0, "y": 0.16, "z": 0 },
                "type": "Sphere",
            });
        }

        var eraserID = this.findEntityIDByName("voxel paint eraser");
        if (!eraserID) {
            eraserID = Entities.addEntity({
                "collidesWith": "",
                "collisionMask": 0,
                "color": { "blue": 0, "green": 0, "red": 255 },
                "dimensions": { "x": 0.03, "y": 0.3, "z": 0.03 },
                "name": "voxel paint eraser",
                "position": eraseBrushPosition,
                "rotation": {
                    "w": 0.60332643985748291,
                    "x": 0.62716102600097656,
                    "y": 0.44969868659973145,
                    "z": -0.20100706815719604
                },
                "script": "http://23.253.109.180/~seth/hifi/voxel-paint/voxel-paint-eraser.js",
                "shape": "Cube",
                "type": "Box",
                "userData": "{\"grabbableKey\":{\"grabbable\":true},\"wearable\":{\"joints\":{\"LeftHand\":[{\"x\":-0.10801754891872406,\"y\":0.15447449684143066,\"z\":0.030637264251708984},{\"x\":-0.32700979709625244,\"y\":0.623619794845581,\"z\":0.28943854570388794,\"w\":0.6483823657035828}],\"RightHand\":[{\"x\":0.11031082272529602,\"y\":0.19449540972709656,\"z\":0.0405043363571167},{\"x\":0.2807741165161133,\"y\":0.6332069635391235,\"z\":0.2997693121433258,\"w\":-0.6557632088661194}]}}}"
            });

            Entities.addEntity({
                "collidesWith": "",
                "collisionMask": 0,
                "color": { "blue": 0, "green": 0, "red": 252 },
                "dimensions": { "x": 0.05, "y": 0.05, "z": 0.05 },
                "name": "voxel paint eraser tip",
                "parentID": eraserID,
                "localPosition": { "x": 0, "y": 0.16, "z": 0 },
                "type": "Sphere",
            });
        }
    };


    this.getPolyVox = function (x, y, z) {
        if (!this.polyvoxes) {
            return null;
        }
        if (!this.polyvoxes[x]) {
            return null;
        }
        if (!this.polyvoxes[x][y]) {
            return null;
        }
        return this.polyvoxes[x][y][z];
    };


    this.linkNeighbors = function () {
        // link all the polyvoxes to their neighbors
        for (var x = 0; x < this.slices; x++) {
            for (var y = 0; y < this.slices; y++) {
                for (var z = 0; z < this.slices; z++) {
                    var polyvox = this.getPolyVox(x, y, z);
                    if (polyvox) {
                        var neighborProperties = {};
                        if (x > 0) {
                            var xNNeighborID = this.getPolyVox(x - 1, y, z);
                            if (xNNeighborID) {
                                neighborProperties.xNNeighborID = xNNeighborID;
                            }
                        }
                        if (x < this.slices - 1) {
                            var xPNeighborID = this.getPolyVox(x + 1, y, z);
                            if (xPNeighborID) {
                                neighborProperties.xPNeighborID = xPNeighborID;
                            }
                        }
                        if (y > 0) {
                            var yNNeighborID = this.getPolyVox(x, y - 1, z);
                            if (yNNeighborID) {
                                neighborProperties.yNNeighborID = yNNeighborID;
                            }
                        }
                        if (y < this.slices - 1) {
                            var yPNeighborID = this.getPolyVox(x, y + 1, z);
                            if (yPNeighborID) {
                                neighborProperties.yPNeighborID = yPNeighborID;
                            }
                        }
                        if (z > 0) {
                            var zNNeighborID = this.getPolyVox(x, y, z - 1);
                            if (zNNeighborID) {
                                neighborProperties.zNNeighborID = zNNeighborID;
                            }
                        }
                        if (z < this.slices - 1) {
                            var zPNeighborID = this.getPolyVox(x, y, z + 1);
                            if (zPNeighborID) {
                                neighborProperties.zPNeighborID = zPNeighborID;
                            }
                        }
                        Entities.editEntity(polyvox, neighborProperties);
                    }
                }
            }
        }
    };


    this.clearVoxelPaintSpace = function () {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 30);
        this.polyvoxes = {};
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == "voxel paint") {
                Entities.deleteEntity(nearbyID);
            }
        }
    };


    this.createVoxel = function (x, y, z) {
        var platformID = this.findEntityIDByName("voxel paint floor");
        var platformProps = Entities.getEntityProperties(platformID, ['position', 'dimensions']);
        var platformDimensions = platformProps.dimensions;
        var platformHalfDimensions = Vec3.multiply(platformDimensions, 0.5);
        var platformCorner = Vec3.subtract(platformProps.position, platformHalfDimensions);

        platformDimensions.y = platformDimensions.x;

        var sliceSize = Vec3.multiply(platformDimensions, 1.0 / this.slices);
        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);

        var position = Vec3.sum({x: platformCorner.x + (x * sliceSize.x),
                                 y: platformCorner.y + (y * sliceSize.y),
                                 z: platformCorner.z + (z * sliceSize.z)},
                                halfSliceSize);

        if (!this.polyvoxes) {
            this.polyvoxes = {};
        }
        if (!this.polyvoxes[x]) {
            this.polyvoxes[x] = {};
        }
        if (!this.polyvoxes[x][y]) {
            this.polyvoxes[x][y] = {};
        }
        this.polyvoxes[x][y][z] = Entities.addEntity({
            type: "PolyVox",
            name: "voxel paint",
            position: position,
            dimensions: sliceSize,
            voxelVolumeSize: { x: 16, y: 16, z: 16 },
            voxelSurfaceStyle: 0,
            collisionless: true,
            xTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg",
            yTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg",
            zTextureURL: "http://headache.hungry.com/~seth/hifi/wood.jpg"
            // xTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg",
            // yTextureURL: "http://headache.hungry.com/~seth/hifi/grass.png",
            // zTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg"
        });

        this.linkNeighbors(this.polyvoxes);

        Entities.setVoxel(this.polyvoxes[x][y][z], {x:8, y:8, z:8}, 255);
    };


    this.createVoxelPaintSpace = function () {
        this.polyvoxes = {};

        for (var x = 0; x < this.slices; x++) {
            for (var y = 0; y < this.slices; y++) {
                for (var z = 0; z < this.slices; z++) {
                    this.createVoxel(x, y, z);
                }
            }
        }
    };

    this.activateWithBaton = function () {
        var state = getEntityCustomData("state", this.entityID, "off");
        if (state == "off") {
            Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 255 }});
            state = "on";
            this.createVoxelPaintSpace();
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
