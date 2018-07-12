
/* global Entities, Vec3, Script, Settings, Model, Assets */

(function () {

    function retryingVoxelsToMesh(entityID, callback) {
        // if the polyvox mesh isn't yet computed, Entities.voxelsToMesh will start the process and call-back
        // with success set to false.
        Entities.getMeshes(entityID, function(meshes, success) {
            if (success) {
                callback(meshes, true);
            } else {
                print("VOXEL-PAINT-EXPORT --  retrying " + entityID);
                Script.setTimeout(function() {
                    retryingVoxelsToMesh(entityID, callback);
                }, 50);
            }
        });
    }

    this.preload = function (entityID) {
        this.entityID = entityID;
    };

    this.findEntityIDByName = function (entityName) {
        var myProperties = Entities.getEntityProperties(this.entityID, ["position", "rotation"]);
        var nearbyEntities = Entities.findEntities(myProperties.position, 8);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ["name"]).name;
            if (nearbyName == entityName) {
                return nearbyID;
            }
        }
        return null;
    };

    this.activate = function () {
        var aetherID = this.findEntityIDByName("voxel paint aether");
        var childIDs = Entities.getChildrenIDs(aetherID);
        var _this = this;

        this.meshesInFlight = 0;

        this.meshes = [];
        childIDs.forEach(function(voxelID) {
            var props = Entities.getEntityProperties(voxelID, ["name"]);
            if (props.name == "voxel paint") {
                print("VOXEL-PAINT-EXPORT --  converting " + voxelID + ", name = " + props.name);
                _this.meshesInFlight += 1;
                retryingVoxelsToMesh(voxelID, function(meshesForThisPolyVox, success) {
                // Entities.getMeshes(voxelID, function(meshesForThisPolyVox, success) {
                    _this.meshesInFlight -= 1;
                    if (success && meshesForThisPolyVox.length > 0) {
                        print("VOXEL-PAINT-EXPORT --  transforming " + voxelID);
                        print("meshes = " + JSON.stringify(meshesForThisPolyVox));
                        var meshTransformed =
                            Model.transformMesh(Entities.getEntityLocalTransform(voxelID), meshesForThisPolyVox[0]);
                        _this.meshes.push(meshTransformed);
                    } else {
                        print("VOXEL-PAINT-EXPORT --  failed on mesh for " + voxelID);
                    }
                });
            }
        });

        Script.setTimeout(function() {
            _this.finish(aetherID);
        }, 500);
    };

    this.finish = function (aetherID) {
        var _this = this;

        print("VOXEL-PAINT-EXPORT --  finish -- mesh count is " + this.meshes.length +
              " in flight is " + this.meshesInFlight);

        if (this.meshesInFlight > 0) {
            Script.setTimeout(function() {
                _this.finish(aetherID);
            }, 50);
            return;
        }

        var aetherProps = Entities.getEntityProperties(aetherID);

        print("--- calling Model.appendMeshes on " + this.meshes.length + " meshes");

        var combinedMeshes = Model.appendMeshes(this.meshes);

        print("--- done combining meshes");

        var objData = Model.meshToOBJ([combinedMeshes]);

        print("--- objData.length = " + objData.length);

        var nth = Math.floor((Math.random() * 10000) + 1);
        var fileName = "/voxel-paint-save-" + nth + ".obj";

        Assets.uploadData(objData, function(url, hash) {
            print("VOXEL-PAINT-EXPORT --  upload is done");
            Assets.setMapping(fileName, hash, function() {
                print("VOXEL-PAINT-EXPORT --  voxel-paint save: atp:" + fileName);
                var newEntityID = Entities.addEntity({
                    type: "Model",
                    modelURL: "atp:" + fileName,
                    position: Vec3.sum(aetherProps.position, { x: -4, y: 0, z: 0 }),
                    name: "polyvox mesh extract test model",
                    dynamic: false,
                    collisionless: true
                });
                print("VOXEL-PAINT-EXPORT --  entity added: " + newEntityID);
            });
        });
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
