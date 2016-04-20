
(function() {
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");

    var rocket;

    Rocket = (function() {
        var _this = this;

        this.batonName = null;
        this.baton = null;

        this.doorID = null;
        this.doorSwitchID = null;
        this.maintenanceInterval = null;

        this.rocketVerticalSliceSize = 3.0; // matches global value in 50s-rocket.scad
        this.rocketRotationalSliceCount = 20; // matches global value in 50s-rocket.scad

        // matches rocket_outline in 50s-rocket.scad
        this.baseRocketRadius = [3.8, 4.6, 5.0, 5.4, 5.6, 5.4, 5.0, 4.2, 3.4, 2.0, 0.2];

        this.rocketWallThickness = 0.1; // matches rocket_wall_thickness in 50s-rocket.scad
        this.sliceRadians = 2.0 * Math.PI / this.rocketRotationalSliceCount;
        this.halfSliceRadians = this.sliceRadians / 2.0;
        this.rocketThrusterOffset = [0, -1.75, -7];
        this.rocketThrusterHeight = 2;

        this.channelKey = null;

        // constants that affect door behavior
        this.doorOpenness = 0.0;
        this.doorSpeed = 0.008;
        this.doorDirection = this.doorSpeed;
        this.doorMoving = false;
        this.doorOpenMax = Math.PI * 106.4 / 180.0;
        this.doorMoveInterval = 40;
        this.doorSwingInterval = null;

        this.vec3toStr = function(v, digits) {
            if (!digits) { digits = 3; }
            return "{ " + v.x.toFixed(digits) + ", " + v.y.toFixed(digits) + ", " + v.z.toFixed(digits)+ " }";
        }

        this.randomize = function(number, variability) {
            var allowedDeviation = number * variability; // one side of the deviation range
            var allowedDeviationRange = allowedDeviation * 2; // total range for +/- deviation
            var randomDeviation = Math.random() * allowedDeviationRange;
            var result = number - allowedDeviation + randomDeviation;
            return result;
        }

        this.preload = function(entityID) {
            this.rocketID = entityID;
            this.calculateRocketOffset();

            this.channelKey = this.rocketID;

            this.maintenanceInterval = Script.setInterval(function() {
                _this.findRemote();
            }, 5000);

            this.batonName = 'io.highfidelity.seth.50sRocket:' + this.rocketID;
            this.baton = acBaton({
                batonName: this.batonName,
                timeScale: 15000
            });
        };

        this.handleMessages = function(id, params) {
            message = params[0];
            print("got message: '" + message + "'");

            var data = null;
            try {
                data = JSON.parse(message);
            } catch (e) {
                return;
            }

            if (data && data.action) {
                if (data.action == 'door') {
                    this.toggleDoor();
                }
            }
        }

        this.doMaintenance = function() {
            _this.maintainDoor();
            _this.findRemote();
        }

        this.maintainDoor = function() {
            if (this.doorID == null) {
                this.doorID = this.findDoor();
                if (this.doorID == null) {
                    print("50s-rocket -- can't find door");
                    return;
                }
            }

            // decide if the door was left open or closed
            var localRotation = Entities.getEntityProperties(this.doorID, ["rotation", "localRotation"]).localRotation;
            var rotationRPY = Quat.safeEulerAngles(localRotation);
            if (rotationRPY.x < this.doorOpenMax / 2) {
                this.doorOpenness = 0.0;
                this.doorDirection = this.doorSpeed;
            } else {
                this.doorOpenness = 1.0;
                this.doorDirection = -this.doorSpeed;
            }
            this.doorMoving = false;
            Entities.callEntityMethod(this.doorID, "setChannelKey", [this.channelKey]);
        }

        this.findRemote = function() {
            var rocketProperties = Entities.getEntityProperties(this.rocketID, ['position', 'rotation']);
            var rocketScadPosition = rocketProperties.position;
            var nearbyEntities = Entities.findEntities(rocketScadPosition, 100);
            var success = false;
            for (i = 0; i < nearbyEntities.length; i++) {
                var nearbyID = nearbyEntities[i];
                var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
                if (nearbyName == '50s rocket remote door opener') {
                    Entities.callEntityMethod(nearbyID, "setChannelKey", [this.channelKey]);
                    success = true;
                }
            }
            return success;
        }

        this.findDoor = function() {
            var rocketProperties = Entities.getEntityProperties(this.rocketID, ['position', 'rotation']);
            var rocketScadPosition = rocketProperties.position;
            var nearbyEntities = Entities.findEntities(rocketScadPosition, this.baseRocketRadius[1]);
            for (i = 0; i < nearbyEntities.length; i++) {
                var nearbyID = nearbyEntities[i];
                var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
                if (nearbyName == '50s rocket door') {
                    return nearbyID;
                }
            }
            return null;
        }

        this.calculateRocketOffset = function() {
            var rocketBodyHeight = this.rocketVerticalSliceSize * 10;
            var thrusterTallness = (this.rocketThrusterHeight / 2.0) - this.rocketThrusterOffset[1];

            // make the rocket-body's registration point line up with openscad's idea of the model-local origin
            var offset = {
                x: 0, // the thruster pads are symetric along z axis
                // upper surface of the lower floor is y origin
                y: (rocketBodyHeight + thrusterTallness) / 2 - thrusterTallness,
                z: -1.75 // the thruster pads aren't symetric along z axis
            };
            var dimensions = { x: 17.124,
                               y: rocketBodyHeight + thrusterTallness, // 32.75
                               z: 15.473 };

            Entities.editEntity(this.rocketID, {registrationPoint: {x: (dimensions.x / 2.0 - offset.x) / dimensions.x,
                                                                    y: (dimensions.y / 2.0 - offset.y) / dimensions.y,
                                                                    z: (dimensions.z / 2.0 - offset.z) / dimensions.z}});
        }

        this.clickDownOnEntity = function(entityID, mouseEvent) {
            // this.toggleDoor();
        };

        this.toggleDoor = function() {
            _this.baton.claim(
                function () {
                    _this.doMaintenance();
                    _this.toggleDoorWBaton();
                }
            );
        }

        this.toggleDoorWBaton = function() {
            if (this.doorID == null) {
                return;
            }
            if (_this.doorMoving) {
                return;
            }
            _this.doorMoving = true;
            _this.doorSwingInterval = Script.setInterval(function() {
                _this.doorOpenness += _this.doorDirection;
                if (_this.doorOpenness < 0.0) {
                    _this.doorOpenness = 0.0;
                    _this.doorDirection = -_this.doorDirection;
                    Script.clearInterval(_this.doorSwingInterval);
                    _this.doorMoving = false;
                    _this.baton.release();
                }
                if (_this.doorOpenness > 1.0) {
                    _this.doorOpenness = 1.0;
                    _this.doorDirection = -_this.doorDirection;
                    Script.clearInterval(_this.doorSwingInterval);
                    _this.doorMoving = false;
                    _this.baton.release();
                }
                _this.positionDoor(_this.doorOpenness);
            }, _this.doorMoveInterval);
        }

        this.calculateDoorPosition = function(opennessRatio) {
            var p0 = {
                x: Math.sin(0.0) * this.baseRocketRadius[0],
                y: 0,
                z: Math.cos(0.0) * this.baseRocketRadius[0]
            };
            var p1 = {
                x: Math.sin(this.sliceRadians) * this.baseRocketRadius[0],
                y: 0,
                z: Math.cos(this.sliceRadians) * this.baseRocketRadius[0]
            };
            var rampRotation = Quat.fromPitchYawRollRadians(this.doorOpenMax * opennessRatio, this.halfSliceRadians, 0);
            var doorZDimension = this.baseRocketRadius[2] - (this.baseRocketRadius[0] - this.rocketWallThickness);
            return {
                registrationPoint: { x: 0.5, y: 0.0, z: (this.rocketWallThickness / doorZDimension) },
                localPosition: Vec3.multiply(Vec3.sum(p0, p1), 0.5),
                localRotation: rampRotation
            };
        }

        this.positionDoor = function(opennessRatio) {
            if (this.doorID == null) {
                return;
            }

            Entities.editEntity(this.doorID, this.calculateDoorPosition(opennessRatio));
        };

        this.unload = function() {
            if (this.doorSwingInterval) {
                Script.clearInterval(this.doorSwingInterval);
            }
            if (this.maintenanceInterval) {
                Script.clearInterval(this.maintenanceInterval);
            }
        };
    });

    Rocket.prototype = {
        handleMessage: function(id, params) {
            rocket.handleMessages(id, params);
        }
    }

    rocket = new Rocket();
    return rocket;
});
