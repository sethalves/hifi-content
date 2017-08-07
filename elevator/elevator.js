
/*global Entities, Script, Vec3 */

(function() {

    var self = this;
    var timeSliceMilliseconds = 1000;

    self.moveElevator = function () {
        var elevatorProperties = Entities.getEntityProperties(self.elevatorID, ["position", "velocity", "userData"]);
        var elevatorPosition = elevatorProperties.position;
        var elevatorVelocity = elevatorProperties.velocity;

        var elevatorData;
        try {
            elevatorData = JSON.parse(elevatorProperties.userData).elevatorData;
        } catch (err) {
            print("Elevator not configured: " + err);
            return;
        }

        var elevatorBasePosition = elevatorData.basePosition;
        var elevatorRise = elevatorData.rise;
        var zoneHeight = elevatorData.zoneHeight;
        var zoneVerticalOffset = elevatorData.zoneVerticalOffset;
        var vertLow = elevatorBasePosition.y + zoneVerticalOffset + (zoneHeight / 2);
        var vertHigh = vertLow + elevatorRise;

        var newVelocity = {x: 0, y: 0, z: 0};
        var velocityNeedsUpdate = false;

        if (elevatorPosition.y <= vertLow && elevatorVelocity.y < 0) {
            newVelocity.y = 1.0;
            velocityNeedsUpdate = true;
        } else if (elevatorPosition.y >= vertHigh && elevatorVelocity.y > 0.0) {
            newVelocity.y = -1.0;
            velocityNeedsUpdate = true;
        } else if (Vec3.length(elevatorVelocity) < 0.5) {
            newVelocity.y = 1.0;
            velocityNeedsUpdate = true;
        }

        if (velocityNeedsUpdate) {
            Entities.editEntity(self.elevatorID, {
                velocity: newVelocity,
                localizedSimulation: true
            });
        }

        var untilNextCheck = timeSliceMilliseconds;
        if (Math.abs(elevatorPosition.y - vertLow) < 1.0 ||
            Math.abs(elevatorPosition.y - vertHigh) < 1.0) {
            // check more often if it's nearly time to turn around
            untilNextCheck = timeSliceMilliseconds / 10;
        }

        Script.setTimeout(function () {
            self.moveElevator();
        }, untilNextCheck);
    };

    self.preload = function (entityID) {
        self.elevatorID = entityID;

        Script.setTimeout(function () {
            self.moveElevator();
        }, timeSliceMilliseconds);
    };
});
