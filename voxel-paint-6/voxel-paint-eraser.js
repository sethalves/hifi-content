
/* global Entities, Script */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");

    return genericTool.genericTool(
        function() { // start
            this.brush = Entities.getChildrenIDs(this.entityID)[0];
        },
        function() { // continue
            var brushProps = Entities.getEntityProperties(this.brush, ["position", "rotation",
                                                                       "dimensions", "registrationPoint"]);
            var searchRadius = 2.0;
            var editSphereRadius = brushProps.dimensions.x / 2.0;

            var ids = Entities.findEntities(brushProps.position, searchRadius);
            for (var i = 0; i < ids.length; i++) {
                Entities.setVoxelSphere(ids[i], brushProps.position, editSphereRadius, 0);
            }
        },
        null); // stop
});
