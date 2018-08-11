"use strict";

/* global Script, Entities, MyAvatar, print */

(function() {
    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;

    var allIDs = Entities.findEntities(MyAvatar.position, 1000);
    var toExportIDs = [];
    for (var i = 0; i < allIDs.length; i++) {
        var entityID = allIDs[ i ];
        var props = Entities.getEntityProperties(entityID);
        if (props.locked ||
            props.name == "dummy" ||
            props.id == "{8632732d-03da-4f36-b100-ae1934042f22}" || // chest box
            props.id == "{e16144b1-b801-461d-99e6-471862e64542}" || // chest lid
            props.name == "table with drawers" ||
            props.modelURL == "http://headache.hungry.com/~seth/hifi/table/table-drawer.obj" ||
            props.script == "https://s3-us-west-1.amazonaws.com/hifi-content/clement/production/scripts/sit.js"
           ) {

            if (props.id == "{df662b84-8273-444a-bd14-b7dca86ef3ab}" || // old terrain
                props.id == "{b75cd8c5-5203-4fc6-a1de-26173c789d8b}" || // cube at 8000,8000,8000
                props.id == "{b5151e2f-bd84-44f5-a9b9-84fc542007a3}" || // boat
                props.name == "Alan water" ||
                props.name == "Trees Skybox" ||
                props.name == "Moon") {
                continue;
            }

            toExportIDs.push(entityID);
        }
    }

    var asProps = entitiesIDsToProperties(toExportIDs, { x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 0, w: 1 });
    var asJSON = JSON.stringify(asProps);
    print(asJSON);
})();
