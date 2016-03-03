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
    this.doorID = null;
    this.doorSwitchID = null;
    this.findPartsInterval = null;

    this.rocketVerticalSliceSize = 3.0; // matches global value in 50s-rocket.scad
    this.rocketRotationalSliceCount = 20; // matches global value in 50s-rocket.scad
    this.baseRocketRadius = [4.0, 4.6, 5.0, 5.4, 5.6, 5.4, 5.0, 4.2, 3.4, 2.0, 0.2]; // matches rocket_outline in 50s-rocket.scad
    this.rocketWallThickness = 0.1; // matches rocket_wall_thickness in 50s-rocket.scad
    this.sliceRadians = 2.0 * Math.PI / this.rocketRotationalSliceCount;
    this.halfSliceRadians = this.sliceRadians / 2.0;
    this.rocketThrusterOffset = [0, -1.75, -7];
    this.rocketThrusterHeight = 2;

    // constants that affect door behavior
    this.doorOpenness = 0.0;
    this.doorDirection = -0.008;
    this.doorMoving = false;
    this.doorOpenMax = Math.PI * 108.5 / 180.0;
    this.doorMoveInterval = 40;
    this.doorSwingInterval = null;

    this.vec3toStr = function(v, digits) {
        if (!digits) { digits = 3; }
        return "{ " + v.x.toFixed(digits) + ", " + v.y.toFixed(digits) + ", " + v.z.toFixed(digits)+ " }";
    }

    this.findParts = function() {
        var _this = this;
        this.findPartsInterval = Script.setInterval(function() {
            var rocketProperties = Entities.getEntityProperties(_this.rocketID, ['position', 'rotation']);
            var nearbyEntities = Entities.findEntities(rocketProperties.position, 20.0);
            for (i = 0; i < nearbyEntities.length; i++) {
                var nearbyID = nearbyEntities[i];
                var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
                // print("checking: " + nearbyID + " " + nearbyName);
                if (nearbyName == '50s rocket door') {
                    _this.doorID = nearbyID;
                }
                if (nearbyName == '50s rocket door switch') {
                    _this.doorSwitchID = nearbyID;
                }
            }
            if (_this.doorID != null
                // && _this.doorSwitchID != null
               ) {
                Script.clearInterval(_this.findPartsInterval);
                _this.positionDoor(_this.doorOpenness);
            }
        }, 200);
    };

    this.preload = function(entityId) {
        // figure out entityIDs for moving parts
        this.rocketID = entityId;

        this.findParts();

        // openscad space + rocket-offset = hifi space
        this.calculateDoorOffset();
        this.calculateRocketOffset();
    };


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

        print("total Height = " + (rocketBodyHeight + thrusterTallness));
        print("thrusterTallness = " + thrusterTallness);

        this.rocketOffset = {
            x: 0,
            y: (rocketBodyHeight + thrusterTallness) / 2 - thrusterTallness,
            z: -1.75
        };
    }

    this.clickDownOnEntity = function(entityID, mouseEvent) {
        this.toggleDoor();
    };

    this.toggleDoor = function() {
        print("toggleDoor");
        if (this.doorID == null) {
            print("this.doorID == null");
            return;
        }
        if (this.doorMoving) {
            print("this.doorMoving");
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

    this.positionDoor = function(opennessRatio) {
        if (this.doorID == null) {
            return;
        }
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

        Entities.editEntity(this.doorID, {
            parentID: this.rocketID,
            parentJointIndex: -1,
            localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, adjustmentDueToRotation),
            localRotation: rampRotation,
            collidesWith: "static,dynamic,kinematic,myAvatar,otherAvatar"
        });

        // Entities.addEntity({
        //     name: '50s rocket debug',
        //     type: 'Sphere',
        //     parentID: this.rocketID,
        //     parentJointIndex: -1,
        //     localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampPivot),
        //     dimensions: { x: 0.15, y: 0.15, z: 0.15 }
        // });
    };

    this.unload = function() {
        // Script.update.disconnect(this.update);
        if (this.doorSwingInterval) {
            Script.clearInterval(this.doorSwingInterval);
        }
        this.cleanUp();
    };

    this.cleanUp = function() {
        // Entities.deleteEntity(this.light);
        // Overlays.deleteOverlay(this.field);
    };

    // Script.scriptEnding.connect(this.scriptEnding);
    // Script.update.connect(this.update);
});
