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
    this.NULL_UUID = "{00000000-0000-0000-0000-000000000000}";

    this.doorID = null;
    this.doorSwitchID = null;
    // this.findPartsInterval = null;
    this.maintenanceInterval = null;

    this.rocketVerticalSliceSize = 3.0; // matches global value in 50s-rocket.scad
    this.rocketRotationalSliceCount = 20; // matches global value in 50s-rocket.scad
    this.baseRocketRadius = [4.0, 4.6, 5.0, 5.4, 5.6, 5.4, 5.0, 4.2, 3.4, 2.0, 0.2]; // matches rocket_outline in 50s-rocket.scad
    this.rocketWallThickness = 0.1; // matches rocket_wall_thickness in 50s-rocket.scad
    this.sliceRadians = 2.0 * Math.PI / this.rocketRotationalSliceCount;
    this.halfSliceRadians = this.sliceRadians / 2.0;
    this.rocketThrusterOffset = [0, -1.75, -7];
    this.rocketThrusterHeight = 2;

    this.channelKey = null;

    // constants that affect door behavior
    this.doorOpenness = 0.0;
    this.doorDirection = 0.008;
    this.doorMoving = false;
    this.doorOpenMax = Math.PI * 108.5 / 180.0;
    this.doorMoveInterval = 40;
    this.doorSwingInterval = null;

    this.vec3toStr = function(v, digits) {
        if (!digits) { digits = 3; }
        return "{ " + v.x.toFixed(digits) + ", " + v.y.toFixed(digits) + ", " + v.z.toFixed(digits)+ " }";
    }

    // this.findParts = function() {
    //     var _this = this;
    //     this.findPartsInterval = Script.setInterval(function() {
    //         var rocketProperties = Entities.getEntityProperties(_this.rocketID, ['position', 'rotation']);
    //         var nearbyEntities = Entities.findEntities(rocketProperties.position, 20.0);
    //         for (i = 0; i < nearbyEntities.length; i++) {
    //             var nearbyID = nearbyEntities[i];
    //             var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
    //             // print("checking: " + nearbyID + " " + nearbyName);
    //             if (nearbyName == '50s rocket door') {
    //                 _this.doorID = nearbyID;
    //             }
    //             if (nearbyName == '50s rocket door switch') {
    //                 _this.doorSwitchID = nearbyID;
    //             }
    //         }
    //         if (_this.doorID != null
    //             // && _this.doorSwitchID != null
    //            ) {
    //             Script.clearInterval(_this.findPartsInterval);
    //             _this.positionDoor(_this.doorOpenness);
    //         }
    //     }, 200);
    // };

    this.preload = function(entityId) {
        var _this = this;
        this.rocketID = entityId;

        // openscad space + rocket-offset = hifi space
        this.calculateDoorOffset();
        this.calculateRocketOffset();

        this.channelKey = '555abc';
        Messages.subscribe(this.channelKey);
        Messages.messageReceived.connect(function(channel, message, sender) {
            _this.handleMessages(channel, message, sender);
        });
        this.maintenanceInterval = Script.setInterval(function() {
            _this.doMaintenance();
        }, 3000);

        this.doMaintenance();
        this.toggleDoor(); // so it starts closed and initially opens
    };

    this.handleMessages = function(channel, message, sender) {
        // if (sender === MyAvatar.sessionUUID) {

        print("got message, channel='" + channel + "', message='" + message + "'");

        if (channel != this.channelKey) {
            return;
        }

        var data = null;
        try {
            data = JSON.parse(message);
        } catch (e) {
            return;
        }

        if (data && data.action) {
            if (data.action == 'remote clicked') {
                this.toggleDoor();
            }
        }
    }

    this.doMaintenance = function() {
        this.maintainDoor();
        this.maintainRemote();
    }

    this.maintainDoor = function() {
        if (this.doorID == null) {
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
            doorProperties["lifetime"] = 15;
            this.doorID = Entities.addEntity(doorProperties);

            // reset the door size
            // var previousDoorProps = Entities.getEntityProperties(this.doorID, ["type", "naturalDimensions"]);
            // var naturalDimensions = previousDoorProps.naturalDimensions;
            // if (previousDoorProps.type == "Model" &&
            //     naturalDimensions.x != 0 && naturalDimensions.y != 0 && naturalDimensions.z != 0) {
            //     Entities.editEntity(this.doorID, {
            //         dimensions: previousDoorProps.naturalDimensions,
            //         collidesWith: "static,dynamic,kinematic,myAvatar,otherAvatar"
            //     });
            // }
        } else {
            var doorProperties = Entities.getEntityProperties(this.doorID, ["name", "age"]);
            if (doorProperties.name == '50s rocket door') {
                doorProperties = this.calculateDoorPosition(this.doorOpenness);
                doorProperties["lifetime"] = doorProperties.age + 15;
                Entities.editEntity(this.doorID, doorProperties);
            }
        }
    }

    this.findRemote = function() {
        var rocketProperties = Entities.getEntityProperties(this.rocketID, ['position', 'rotation']);
        var rocketScadPosition = Vec3.subtract(rocketProperties.position, this.rocketOffset);
        var nearbyEntities = Entities.findEntities(rocketScadPosition, this.baseRocketRadius[1]);
        for (i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == '50s rocket remote door opener') {
                Entities.callEntityMethod(nearbyID, "setChannelKey", [this.channelKey]);
                return nearbyID;
            }
        }
        return null;
    }

    this.maintainRemote = function() {
        if (this.findRemote()) {
            return;
        }
        // we didn't find the opener
        print("50s rocket creating new remote door opener");
        var remoteID = Entities.addEntity({
            type: "Box",
            name: '50s rocket remote door opener',
            localPosition: Vec3.sum(Vec3.multiply(this.rocketOffset, -1.0), { x: 1.5, y: 1.5, z: 0 }),
            parentID: this.rocketID,
            parentJointIndex: -1,
            dimensions: { x: 0.08, y: 0.08, z: 0.08 },
            color: { red: 200, green: 0, blue: 20 },
            shapeType: 'box',
            dynamic: true,
            gravity: { x: 0, y: -5.0, z: 0 },
            restitution: 0,
            damping: 0.5,
            lifetime: 3600,
            collisionSoundURL: "http://hifi-content.s3.amazonaws.com/james/pistol/sounds/drop.wav",
            script: 'http://headache.hungry.com/~seth/hifi/50s-rocket-remote.js',
            userData: JSON.stringify({
                grabbableKey: { grabbable: true },
                wearable:{joints:{RightHand:[{x:0.07079616189002991,
                                              y:0.20177987217903137,
                                              z:0.06374628841876984},
                                             {x:-0.5863648653030396,
                                              y:-0.46007341146469116,
                                              z:0.46949487924575806,
                                              w:-0.4733745753765106}],
                                  LeftHand:[{x:0.1802254319190979,
                                             y:0.13442856073379517,
                                             z:0.08504903316497803},
                                            {x:0.2198076844215393,
                                             y:-0.7377811074256897,
                                             z:0.2780133783817291,
                                             w:0.574519157409668}]}}
            })
        });
        Entities.editEntity(remoteID, {parentID: this.NULL_UUID});
        Entities.callEntityMethod(remoteID, "setChannelKey", this.channelKey);
    }

    this.calculateDoorOffset = function() {
        // figure out the offset from the registration-point of the door to its rotation point

        var lowZ = this.baseRocketRadius[0] - this.rocketWallThickness;
        var highZ = this.baseRocketRadius[2];
        var zSize = highZ - lowZ;
        // the origin in door-space is the point about which it rotates.  I would change the registration point,
        // but it all goes wrong.
        var zOffset = zSize / 2.0 - this.rocketWallThickness;

        this.doorOffset = {
            x: 0,
            y: -this.rocketVerticalSliceSize, // door is 2 slices high
            z: - zOffset
        };
    };

    this.calculateRocketOffset = function() {
        var rocketBodyHeight = this.rocketVerticalSliceSize * 10;
        var thrusterTallness = (this.rocketThrusterHeight / 2.0) - this.rocketThrusterOffset[1];

        this.rocketOffset = {
            x: 0,
            y: (rocketBodyHeight + thrusterTallness) / 2 - thrusterTallness,
            z: -1.75
        };
    }

    this.clickDownOnEntity = function(entityID, mouseEvent) {
        // this.toggleDoor();
    };

    this.toggleDoor = function() {
        if (this.doorID == null) {
            return;
        }
        if (this.doorMoving) {
            return;
        }
        this.doorMoving = true;
        var _this = this;
        this.doorSwingInterval = Script.setInterval(function() {
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
        }, this.doorMoveInterval);
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

        // position in openscad rocket's frame...
        var doorScadPositionInLocalRocketScad = Vec3.multiply(Vec3.sum(p0, p1), 0.5);
        // moved by scad --> hifi rocket offset
        var doorScadPositionInLocalRocketHifi = Vec3.subtract(doorScadPositionInLocalRocketScad, this.rocketOffset);
        // moved by scad --> hifi door offset
        var doorHiFiPositionInLocalRocketHifi = Vec3.subtract(doorScadPositionInLocalRocketHifi, this.doorOffset);

        var rampRotation = Quat.fromPitchYawRollRadians(this.doorOpenMax * opennessRatio, this.halfSliceRadians, 0);
        var rampPivot = Vec3.multiplyQbyV(rampRotation, this.doorOffset);
        var adjustmentDueToRotation = Vec3.subtract(doorScadPositionInLocalRocketHifi,
                                                    Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampPivot));



        // Entities.addEntity({
        //     name: '50s rocket debug',
        //     type: 'Sphere',
        //     parentID: this.rocketID,
        //     parentJointIndex: -1,
        //     localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampPivot),
        //     dimensions: { x: 0.15, y: 0.15, z: 0.15 }
        // });


        return {
            localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, adjustmentDueToRotation),
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
        if (this.doorID) {
            Entities.deleteEntity(this.doorID);
        }
        while (true) {
            var remoteID = this.findRemote();
            if (remoteID) {
                Entities.deleteEntity(remoteID);
            } else {
                break;
            }
        }
    };

    // Script.scriptEnding.connect(this.scriptEnding);
    // Script.update.connect(this.update);
});
