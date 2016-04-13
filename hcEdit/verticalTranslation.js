
(function() {
    var _this;
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

