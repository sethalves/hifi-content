

/*global Entities, Script, acBaton, Vec3 */

(function() {
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");

    var Boat = (function() {
        var _this = this;

        this.batonName = null;
        this.baton = null;

        this.moveBoat = function() {
            var boatProperties = Entities.getEntityProperties(this.boatID, ["position", "velocity"]);
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
                _this.baton.claim(
                    function () { // onGrant
                        Entities.editEntity(_this.boatID, {velocity: newVelocity});
                        _this.baton.release();
                    },
                    null, // onRelease
                    null, // onDenied
                    function () { // onNoServerResponse
                        print("no response from baton-server");
                    }
                );
            }
        };

        this.preload = function(entityID) {
            this.boatID = entityID;

            this.maintenanceInterval = Script.setInterval(function() {
                _this.moveBoat();
            }, 1000);

            this.batonName = 'io.highfidelity.seth.Boat:' + this.boatID;
            this.baton = acBaton({
                batonName: this.batonName,
                timeScale: 15000
            });

            this.moveBoat();
        };
    });

    return new Boat();
});
