// "use strict";
//
//
//

// quatToStr = function(q, digits) {
//     if (!digits) { digits = 3; }
//     return "{ " + q.w.toFixed(digits) + ", " + q.x.toFixed(digits) + ", " +
//         q.y.toFixed(digits) + ", " + q.z.toFixed(digits)+ " }";
// }

(function() {
    var rocket;

    Rocket = (function() {
        var _this = this;
        this.NULL_UUID = "{00000000-0000-0000-0000-000000000000}";

        this.HIFI_PUBLIC_BUCKET = "http://s3.amazonaws.com/hifi-public/";
        Script.include(this.HIFI_PUBLIC_BUCKET + "scripts/libraries/virtualBaton.js");
        // Script.include("http://headache.hungry.com/~seth/hifi/virtualBaton.js");
        // this.baton = null;

        this.doorID = null;
        this.doorSwitchID = null;
        // this.findPartsInterval = null;
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

            // this.channelKey = '555abc';
            // Messages.subscribe(this.channelKey);
            // Messages.messageReceived.connect(function(channel, message, sender) {
            //     _this.handleMessages(channel, message, sender);
            // });

            this.channelKey = this.rocketID;

            this.maintenanceInterval = Script.setInterval(function() {
                _this.doMaintenance();
            }, 5000);

            this.baton = virtualBaton({
                batonName: 'io.highfidelity.seth.50sRocket:' + _this.rocketID, // One winner for each entity
                electionTimeout: this.randomize(3000, 0.2),
                recheckInterval: this.randomize(3000, 0.2)
            });
        };

        // this.handleMessages = function(channel, message, sender) {
        this.handleMessages = function(id, params) {
            // if (sender === MyAvatar.sessionUUID) {

            // if (channel != this.channelKey) {
            //     this.baton.messageHandler(channel, message, sender);
            //     return;
            // }

            // print("got message, channel='" + channel + "', message='" + message + "'");

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
            // var _this = this;
            this.baton.claim(
                function() {
                    _this.maintainDoor();
                    // _this.maintainRemote();
                    _this.baton.release();
                },
                function() {
                }
            );
        }

        this.maintainDoor = function() {
            if (this.doorID == null) {
                this.doorID = this.findDoor();
                if (this.doorID == null) {
                    print("50s-rocket -- can't find door");
                    return;
                }
                this.findRemote();

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

                // the door should already be in place, but do an edit to fix it up
                var doorProperties = this.calculateDoorPosition(this.doorOpenness);
                doorProperties["name"] = '50s rocket door';
                doorProperties["type"] = 'Model';
                doorProperties["modelURL"] = 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.obj';
                doorProperties["compoundShapeURL"] = 'http://headache.hungry.com/~seth/hifi/50s-rocket-door-collision-hull.obj';
                doorProperties["dynamic"] = false;
                doorProperties["gravity"] = { x: 0, y: 0, z: 0 };
                doorProperties["angularDamping"] = { x: 0.0, y: 0.0, z: 0.0 };
                doorProperties["parentID"] = this.rocketID;
                doorProperties["parentJointIndex"] = -1;
                doorProperties["collidesWith"] = "static,dynamic,kinematic,myAvatar,otherAvatar";
                doorProperties["userData"] = "{\"grabbableKey\":{\"wantsTrigger\":true}}";
                doorProperties["script"] = 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.js';
                var doorZDimension = this.baseRocketRadius[2] - (this.baseRocketRadius[0] - this.rocketWallThickness);
                doorProperties["registrationPoint"] = { x: 0.5, y: 0.0, z: (this.rocketWallThickness / doorZDimension) };
                Entities.editEntity(this.doorID, doorProperties);

                Entities.callEntityMethod(this.doorID, "setChannelKey", [this.channelKey]);

                // this.toggleDoorWBaton(); // so it starts closed and initially opens
            } else {
                var doorProperties = Entities.getEntityProperties(this.doorID, ["name"]);
                if (doorProperties.name == '50s rocket door') {
                    doorProperties = this.calculateDoorPosition(this.doorOpenness);
                    Entities.editEntity(this.doorID, doorProperties);
                    Entities.callEntityMethod(this.doorID, "setChannelKey", [this.channelKey]);
                } else {
                    this.doorID = null;
                }
            }
        }

        this.findRemote = function() {
            var rocketProperties = Entities.getEntityProperties(this.rocketID, ['position', 'rotation']);
            var rocketScadPosition = rocketProperties.position;
            var nearbyEntities = Entities.findEntities(rocketScadPosition, 60);
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

        // this.maintainRemote = function() {
        //     if (this.findRemote()) {
        //         return;
        //     }
        //     // we didn't find the opener
        //     print("50s rocket creating new remote door opener");
        //     var remoteID = Entities.addEntity({
        //         type: "Box",
        //         name: '50s rocket remote door opener',
        //         localPosition: { x: this.baseRocketRadius[0] - 0.2, y: 1.3, z: 0 },
        //         parentID: this.rocketID,
        //         parentJointIndex: -1,
        //         dimensions: { x: 0.08, y: 0.16, z: 0.08 },
        //         color: { red: 200, green: 0, blue: 20 },
        //         shapeType: 'box',
        //         dynamic: false,
        //         // dynamic: true,
        //         // gravity: { x: 0, y: -1, z: 0 },
        //         // velocity: { x: 0, y: 0.5, z: 0 }, // to make it fall
        //         restitution: 0,
        //         damping: 0.5,
        //         lifetime: 3600,
        //         collisionSoundURL: "http://hifi-content.s3.amazonaws.com/james/pistol/sounds/drop.wav",
        //         script: 'http://headache.hungry.com/~seth/hifi/50s-rocket-remote.js',
        //         userData: JSON.stringify({
        //             grabbableKey: { grabbable: true },
        //             wearable:{joints:{RightHand:[{x:0.07079616189002991,
        //                                           y:0.20177987217903137,
        //                                           z:0.06374628841876984},
        //                                          {x:-0.5863648653030396,
        //                                           y:-0.46007341146469116,
        //                                           z:0.46949487924575806,
        //                                           w:-0.4733745753765106}],
        //                               LeftHand:[{x:0.1802254319190979,
        //                                          y:0.13442856073379517,
        //                                          z:0.08504903316497803},
        //                                         {x:0.2198076844215393,
        //                                          y:-0.7377811074256897,
        //                                          z:0.2780133783817291,
        //                                          w:0.574519157409668}]}}
        //         })
        //     });
        //     Entities.editEntity(remoteID, {parentID: this.NULL_UUID});
        //     Entities.callEntityMethod(remoteID, "setChannelKey", this.channelKey);
        // }

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
            // var _this = this;
            this.baton.claim(
                function() {
                    _this.toggleDoorWBaton();
                    _this.baton.release();
                },
                function() {
                }
            );
        }

        this.toggleDoorWBaton = function() {
            // var _this = this;
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
                }
                if (_this.doorOpenness > 1.0) {
                    _this.doorOpenness = 1.0;
                    _this.doorDirection = -_this.doorDirection;
                    Script.clearInterval(_this.doorSwingInterval);
                    _this.doorMoving = false;
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
            return {
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
            // Script.update.disconnect(this.update);
            if (this.doorSwingInterval) {
                Script.clearInterval(this.doorSwingInterval);
            }
            if (this.maintenanceInterval) {
                Script.clearInterval(this.maintenanceInterval);
            }
            // if (this.doorID) {
            //     Entities.deleteEntity(this.doorID);
            // }
            // while (true) {
            //     var remoteID = this.findRemote();
            //     if (remoteID) {
            //         Entities.deleteEntity(remoteID);
            //     } else {
            //         break;
            //     }
            // }
            if (this.baton) {
                this.baton.unload();
            }
        };

        // Script.scriptEnding.connect(this.scriptEnding);
        // Script.update.connect(this.update);
    });

    Rocket.prototype = {
        handleMessage: function(id, params) {
            rocket.handleMessages(id, params);
        }
    }

    rocket = new Rocket();
    return rocket;
});
