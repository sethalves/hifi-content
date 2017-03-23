//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html

/*global Script, genericTool, Entities, Vec3, Quat */


(function() {
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
