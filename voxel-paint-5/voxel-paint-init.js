
/* global Entities, Vec3, Script, paintBucketColors */

(function () {
    Script.include('/~/system/libraries/utils.js');
    Script.include(Script.resolvePath('voxel-paint-shared.js'));

    this.dimension = 3;

    this.preload = function (entityID) {
        this.entityID = entityID;
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

    this.addPlatform = function (name) {
        var properties = Entities.getEntityProperties(this.entityID, ['position', 'rotation', 'dimensions']);
        var pickRay = {
            origin: properties.position,
            direction: { x: 0, y: -1, z: 0 },
            length: 3
        };
        var intersection = Entities.findRayIntersection(pickRay, true, [], [this.entityID],
                                                        true, // visibleOnly
                                                        true); // collidableOnly
        var platformPosition;
        if (intersection.intersects) {
            platformPosition = Vec3.sum(properties.position, { x: 0, y: 0.01 - intersection.distance, z: 0 });
        } else {
            platformPosition = Vec3.sum(properties.position, { x: 0, y: -1.2, z: 0 });
        }
        return Entities.addEntity({
            color: { red: 75, green: 75, blue: 75 },
            dimensions: { x: this.dimension, y: 0.02, z: this.dimension },
            name: name,
            position: Vec3.sum(platformPosition, Vec3.multiplyQbyV(properties.rotation, {
                x: this.dimension / 2,
                y: 0,
                z: this.dimension / 2
            })),
            rotation: properties.rotation,
            modelURL: MODELS_PATH + 'studioFloor.fbx',
            type: 'Model'
        });
    };


    this.addAether = function (name, platformID) {
        var props = Entities.getEntityProperties(platformID, ['position', 'rotation', 'dimensions']);
        var position = props.position;
        var dimensions = props.dimensions;
        dimensions.y = this.dimension;
        var aetherPosition = Vec3.sum(position, { x: 0, y: this.dimension / 2, z: 0 });

        return Entities.addEntity({
            color: { red: 0, green: 0, blue: 0 },
            dimensions: dimensions,
            name: name,
            position: aetherPosition,
            rotation: props.rotation,
            shape: 'Cube',
            type: 'Box',
            collidesWith: '',
            collisionMask: 0,
            collisionless: true,
            visible: false,
            script: Script.resolvePath('voxel-paint-aether.js'),
            userData: JSON.stringify({ grabbableKey: {grabbable: false} })
        });
    };

    this.addPaletteSpawner = function(position) {

        var properties = Entities.getEntityProperties(this.entityID, ['position', 'rotation', 'dimensions']);
        var pickRay = {
            origin: properties.position,
            direction: { x: 0, y: -1, z: 0 },
            length: 3
        };
        var intersection = Entities.findRayIntersection(pickRay, true, [], [this.entityID],
                                                        true, // visibleOnly
                                                        true); // collidableOnly
        var paletteSpawnerPosition;
        if (intersection.intersects) {
            paletteSpawnerPosition = Vec3.sum(properties.position, { x: 0, y: 1.1 - intersection.distance, z: 0 });
        } else {
            paletteSpawnerPosition = properties.position;
        }
        var paletteSpawnerPosition = Vec3.sum(paletteSpawnerPosition,
                                              Vec3.multiplyQbyV(properties.rotation, { x: 1,  y: 0, z: 1 }));
        var paletteSpawnerID = Entities.addEntity({
            dimensions: { x: 0.63, y: 0.63, z: 0.63 },
            name: 'voxel paint palette spawner',
            position: paletteSpawnerPosition,
            script: Script.resolvePath('handyAttacher.js'),
            type: 'Sphere',
            collidesWith: '',
            collisionMask: 0,
            collisionless: true,
            visible: false,
            userData: JSON.stringify({
                grabbableKey: {
                    wantsTrigger: true
                }
            })
        });
        Entities.addEntity({
            dimensions: {
                x: 0.62574279308319092,
                y: 0.023471139371395111,
                z: 0.52269172668457031
            },
            modelURL: MODELS_PATH + 'painter_Palette.fbx',
            name: 'voxel paint palette spawner model',
            parentID: paletteSpawnerID,
            localPosition: { x: 0, y: 0, z: 0 },
            shapeType: 'none',
            collidesWith: '',
            collisionMask: 0,
            collisionless: true,
            type: 'Model'
        });
        return paletteSpawnerID;
    };

    this.clearVoxelPaintSpace = function () {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 30);
        this.polyvoxes = {};
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyProps = Entities.getEntityProperties(nearbyID, ['name']);
            if (nearbyProps) {
                var nearbyName = nearbyProps.name;
                if (nearbyName && (nearbyName === 'voxel paint' ||
                                   nearbyName === 'voxel paint debug cube' ||
                                   nearbyName === 'voxel paint aether' ||
                                   nearbyName === 'voxel paint palette spawner' ||
                                   nearbyName === 'voxel paint palette' ||
                                   nearbyName === 'voxel paint tool' ||
                                   nearbyName.substring(0, 25) === 'voxel paint color bucket ')) {
                    Entities.deleteEntity(nearbyID);
                }
            }
        }
    };

    this.activate = function () {
        this.clearVoxelPaintSpace();

        var platformName = 'voxel paint floor';
        var platformID = this.findEntityIDByName(platformName);
        if (!platformID) {
            platformID = this.addPlatform(platformName);
        }

        var aetherName = 'voxel paint aether';
        var aetherID = this.findEntityIDByName(aetherName);
        if (!aetherID) {
            aetherID = this.addAether(aetherName, platformID);
        }

        var position = Entities.getEntityProperties(this.entityID, ['position']).position;

        this.addPaletteSpawner(position);
    };

    this.startNearTrigger = function (entityID) {
        this.activate();
    };

    this.stopNearTrigger = function (entityID) {
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        if (!mouseEvent.isLeftButton || Settings.getValue("io.highfidelity.isEditting")) {
            return;
        }
        this.activate();
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
    };
});
