
(function() {
    var _this;
    Script.include("genericTool.js");
    return genericTool(
        null,
        function() {
            var toolPosition = Entities.getEntityProperties(this.entityID, ["position"]).position;
            var toolDelta = Vec3.subtract(toolPosition, this.toolActivationProperties.position);

            var rpy = Quat.safeEulerAngles(this.entityActivationProperties.rotation);
            rpy.y += toolDelta.y * 180;
            rpy.x += toolDelta.x * 180;

            var newRotation = Quat.fromPitchYawRollDegrees(rpy.x, rpy.y, rpy.z);
            Entities.editEntity(this.targetEntity, { rotation: newRotation });
        },
        null);
});
