
(function() {
    var _this;
    Script.include("genericTool.js");
    return genericTool(
        null,
        function() {
            var toolPosition = Entities.getEntityProperties(this.entityID, ["position"]).position;
            var toolDelta = Vec3.subtract(toolPosition, this.toolActivationProperties.position);
            toolDelta.y = 0;
            var newEntityPosition = Vec3.sum(this.entityActivationProperties.position, toolDelta);
            Entities.editEntity(this.targetEntity, { position: newEntityPosition });
        },
        null);
});

