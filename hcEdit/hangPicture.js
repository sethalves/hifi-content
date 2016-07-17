
(function() {
    var _this;
    Script.include("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");

    var MAX_DISTANCE = 2.0;

    return genericTool(
        function() { // start
            var initialTargetProps = Entities.getEntityProperties(this.targetEntity, ["position", "rotation",
                                                                                      "dimensions", "registrationPoint"]);
            var initialTargetPosition = initialTargetProps.position;
            var initialTargetRotation = initialTargetProps.rotation;
            var targetDimensions = initialTargetProps.dimensions;

            // TODO: deal with registationPoint that isn't {x: 0.5, y: 0.5, z: 0.5}
            var leftOffset = {x: targetDimensions.x * -0.5, y: 0, z: targetDimensions.z * 0.5}
            var rightOffset = {x: targetDimensions.x * 0.5, y: 0, z: targetDimensions.z * 0.5}
            var centerOffsetWF = Vec3.multiplyQbyV(initialTargetRotation, {x: 0, y: 0, z: targetDimensions.z * 0.5})
            // TODO: deal with registationPoint that isn't {x: 0.5, y: 0.5, z: 0.5}


            var leftCenter = Vec3.sum(initialTargetPosition, Vec3.multiplyQbyV(initialTargetRotation, leftOffset));
            var rightCenter = Vec3.sum(initialTargetPosition, Vec3.multiplyQbyV(initialTargetRotation, rightOffset));
            var towardWall = Quat.getFront(initialTargetRotation);

            var leftPickRay = { origin: leftCenter, direction: towardWall };
            var rightPickRay = { origin: rightCenter, direction: towardWall };

            var leftIntersection = Entities.findRayIntersection(leftPickRay, true, [], [this.targetEntity, this.entityID]);
            var rightIntersection = Entities.findRayIntersection(rightPickRay, true, [], [this.targetEntity, this.entityID]);

            if (leftIntersection.intersects && rightIntersection.intersects &&
                leftIntersection.distance < MAX_DISTANCE && rightIntersection.distance < MAX_DISTANCE) {
                var finalTargetBackPosition = Vec3.multiply(Vec3.sum(leftIntersection.intersection,
                                                                     rightIntersection.intersection),
                                                            0.5);
                var finalTargetPosition = Vec3.sum(finalTargetBackPosition, centerOffsetWF);
                var finalSideToSide = Vec3.subtract(rightIntersection.intersection, leftIntersection.intersection);
                var finalNormal = Vec3.normalize(Vec3.cross(finalSideToSide, {x: 0, y: 1, z: 0}));
                var finalTargetRotation = Quat.rotationBetween({x:0, y:0, z:1}, finalNormal);
                Entities.editEntity(this.targetEntity, {position: finalTargetPosition, rotation: finalTargetRotation});
            }
        },
        null, // continue
        null); // stop
});
