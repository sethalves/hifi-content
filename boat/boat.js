
/*global Entities, Script, Vec3 */

(function() {

    var self = this;

    self.preload = function (entityID) {
        self.boatID = entityID;
    };

    self.moveBoat = function () {
        var boatProperties = Entities.getEntityProperties(self.boatID, ["position", "velocity"]);
        var boatPosition = boatProperties.position;
        var boatVelocity = boatProperties.velocity;
        var newVelocity = {x: 0, y: 0, z: 0};
        var velocityNeedsUpdate = false;

        if (boatPosition.z < 65.0 && boatVelocity.z < 0.0) {
            newVelocity.z = 1.0;
            velocityNeedsUpdate = true;
        } else if (boatPosition.z > 85.0 && boatVelocity.z > 0.0) {
            newVelocity.z = -1.0;
            velocityNeedsUpdate = true;
        } else if (Vec3.length(boatVelocity) < 0.5) {
            newVelocity.z = 1.0;
            velocityNeedsUpdate = true;
        }

        if (velocityNeedsUpdate) {
            Entities.editEntity(self.boatID, {
                velocity: newVelocity,
                localizedSimulation: true
            });
        }
    };

    Script.setInterval(function () {
        self.moveBoat();
    }, 1000);
});
