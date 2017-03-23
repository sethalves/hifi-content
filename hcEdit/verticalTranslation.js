//  Distributed under the Apache License, Version 2.0.
//  See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html

/*global Script, genericTool, Entities, Vec3 */

(function() {
    Script.include("genericTool.js");
    return genericTool(
        null,
        function() {
            var toolPosition = Entities.getEntityProperties(this.entityID, ["position"]).position;
            // print(vec3toStr(Vec3.subtract(toolPosition, this.toolActivationProperties.position)));
            var toolDelta = Vec3.subtract(toolPosition, this.toolActivationProperties.position);
            toolDelta.x = 0;
            toolDelta.z = 0;
            var newEntityPosition = Vec3.sum(this.entityActivationProperties.position, toolDelta);
            Entities.editEntity(this.targetEntity, { position: newEntityPosition });
        },
        null);
});

