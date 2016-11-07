
/* global Entities, Vec3, Script, acBaton */

(function () {
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");

    this.batonName = null;
    this.baton = null;

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();

        this.batonName = 'io.highfidelity.seth.voxel-paint:' + this.entityID;
        this.baton = acBaton({
            batonName: this.batonName,
            timeScale: 240000,
        });
    };

    this.findEntityIDByName = function (entityName) {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 2);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == entityName) {
                return nearbyID;
            }
            // print("'" + nearbyName + "' != '" + entityName + "'");
        }
        return null;
    };

    this.resetVoxelPaintSpace = function () {
        var slices = 4;

        var platformID = this.findEntityIDByName("voxel paint floor");
        var platformProps = Entities.getEntityProperties(platformID, ['position', 'dimensions']);
        var platformDimensions = platformProps.dimensions;
        var platformHalfDimensions = Vec3.multiply(platformDimensions, 0.5);
        var platformCorner = Vec3.subtract(platformProps.position, platformHalfDimensions);

        platformDimensions.y = platformDimensions.x;

        var sliceSize = Vec3.multiply(platformDimensions, 1.0 / slices);
        var halfSliceSize = Vec3.multiply(sliceSize, 0.5);


        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 10);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == "voxel paint") {
                Entities.deleteEntity(nearbyID);
            }
        }


        var polyvoxes = {};

        for (var x = 0; x < slices; x++) {
            polyvoxes[x] = {};
            for (var y = 0; y < slices; y++) {
                polyvoxes[x][y] = {};
                for (var z = 0; z < slices; z++) {
                    var position = Vec3.sum({x: platformCorner.x + (x * sliceSize.x),
                                             y: platformCorner.y + (y * sliceSize.y),
                                             z: platformCorner.z + (z * sliceSize.z)},
                                            halfSliceSize);
                    print("x=" + x + ", y=" + y + ", z=" + z);
                    polyvoxes[x][y][z] = Entities.addEntity({
                        type: "PolyVox",
                        name: "voxel paint",
                        position: position,
                        dimensions: sliceSize,
                        voxelVolumeSize: { x: 8, y: 8, z: 8 },
                        voxelSurfaceStyle: 0,
                        xTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg",
                        yTextureURL: "http://headache.hungry.com/~seth/hifi/grass.png",
                        zTextureURL: "http://headache.hungry.com/~seth/hifi/dirt.jpeg"
                        // xTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png",
                        // yTextureURL: "http://headache.hungry.com/~seth/hifi/green.png",
                        // zTextureURL: "http://headache.hungry.com/~seth/hifi/brown.png"
                    });
                }
            }
        }
    };

    this.activate = function () {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 0 }});
        var _this = this;
        this.baton.claim(
            function () { // onGrant
                _this.resetVoxelPaintSpace();
                _this.turnOff();
                _this.baton.release();
            });
    };

    this.turnOff = function() {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 0, red: 255 }});
    }

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
