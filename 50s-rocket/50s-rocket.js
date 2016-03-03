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

    this.rocketVerticalSliceSize = 2.0; // matches global value in 50s-rocket.scad
    this.rocketRotationalSliceCount = 20; // matches global value in 50s-rocket.scad
    this.baseRocketRadius = [4.0, 4.6, 5.0, 5.4, 5.6, 5.4, 5.0, 4.2, 3.4, 2.0, 0.2]; // matches rocket_outline in 50s-rocket.scad
    this.rocketWallThickness = 0.1; // matches rocket_wall_thickness in 50s-rocket.scad
    this.sliceRadians = Math.PI * (360.0 / this.rocketRotationalSliceCount) / 180.0; // convert to radians
    this.halfSliceRadians = this.sliceRadians / 2.0;

    this.vec3toStr = function(v, digits) {
        if (!digits) { digits = 3; }
        return "{ " + v.x.toFixed(digits) + ", " + v.y.toFixed(digits) + ", " + v.z.toFixed(digits)+ " }";
    }

    this.preload = function(entityId) {
        // figure out entityIDs for moving parts
        this.rocketID = entityId;
        var rocketProperties = Entities.getEntityProperties(this.rocketID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(rocketProperties.position, 20.0);
        for (i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            // print("checking: " + nearbyID + " " + nearbyName);
            if (nearbyName == '50s rocket door') {
                this.doorID = nearbyID;
            }
            if (nearbyName == '50s rocket door switch') {
                this.doorSwitchID = nearbyID;
            }
        }

        // openscad space + rocket-offset = hifi space
        this.calculateDoorOffset();
        this.calculateRocketOffset();
        this.positionDoor(1.0);
    };

    this.calculateDoorOffset = function() {
        // figure out the offset from the registration-point of the door to its rotation point

        var lowZ = this.baseRocketRadius[0] - this.rocketWallThickness;
        var highZ = this.baseRocketRadius[2];
        var zSize = highZ - lowZ;
        // the origin in door-space is the point about which it rotates.  I would change the registration point,
        // but it all goes wrong.
        var zOffset = zSize / 2.0 - this.rocketWallThickness;

        // var lowX = Math.sin(-this.halfSliceRadians) * this.baseRocketRadius[1];
        // var highX = Math.sin(this.halfSliceRadians) * this.baseRocketRadius[1];
        // var xSize = highX - lowX;
        // // the origin in door-space is the point about which it rotates.  I would change the registration point,
        // // but it all goes wrong.
        // var xOffset = xSize / 2.0;

        // this.doorOffset = {
        //     x: -0.07, // XXX why?
        //     y: -this.rocketVerticalSliceSize, // door is 2 slices high
        //     z: 0.01 - zOffset
        // };

        this.doorOffset = {
            x: 0,
            y: -this.rocketVerticalSliceSize, // door is 2 slices high
            z: - zOffset
        };


    };

    this.calculateRocketOffset = function() {
        this.rocketOffset = {
            x: 0,
            y: (20.1 / 2.0) - this.rocketWallThickness, // XXX
            z: 0
        };
    }

    this.positionDoor = function(opennessRatio) {
        print("DOOR: " + this.doorID);

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

        print(this.vec3toStr(doorScadPositionInLocalRocketScad));
        print(this.vec3toStr(doorScadPositionInLocalRocketHifi));
        print(this.vec3toStr(doorHiFiPositionInLocalRocketHifi));



        // opennessRatio
        var rampRotation = Quat.fromPitchYawRollDegrees(135 * opennessRatio, this.halfSliceRadians, 0);
        var rampPivot = Vec3.multiplyQbyV(rampRotation, this.doorOffset);

        // var rampHingeOffset = Vec3.subtract(doorHiFiPositionInLocalRocketHifi, rampPivot);
        // var rampPosition = Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampHingeOffset);

        var rotationAdjustment = Vec3.subtract(doorScadPositionInLocalRocketHifi,
                                               Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampPivot));

        Entities.editEntity(this.doorID, {
            parentID: this.rocketID,
            parentJointIndex: -1,
            // localPosition: doorHiFiPositionInLocalRocketHifi,
            localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, rotationAdjustment),
            // localRotation: Quat.fromPitchYawRollRadians(0, this.halfSliceRadians, 0)
            localRotation: rampRotation
        });

        Entities.addEntity({
            name: '50s rocket debug',
            type: 'Sphere',
            parentID: this.rocketID,
            parentJointIndex: -1,
            localPosition: Vec3.sum(doorHiFiPositionInLocalRocketHifi, rampPivot),
            dimensions: { x: 0.15, y: 0.15, z: 0.15 }
        });

        Entities.addEntity({
            name: '50s rocket debug',
            type: 'Sphere',
            parentID: this.rocketID,
            parentJointIndex: -1,
            localPosition: doorScadPositionInLocalRocketHifi,
            dimensions: { x: 0.15, y: 0.15, z: 0.15 }
        });

        // Entities.addEntity({
        //     name: '50s rocket debug',
        //     type: 'Sphere',
        //     parentID: this.rocketID,
        //     parentJointIndex: -1,
        //     localPosition: doorHiFiPositionInLocalRocketHifi,
        //     dimensions: { x: 0.15, y: 0.15, z: 0.15 }
        // });
    };

    this.unload = function() {
        // Script.update.disconnect(this.update);
        this.cleanUp();
    };

    this.cleanUp = function() {
        // Entities.deleteEntity(this.light);
        // Overlays.deleteOverlay(this.field);
    };

    // Script.scriptEnding.connect(this.scriptEnding);
    // Script.update.connect(this.update);
});
