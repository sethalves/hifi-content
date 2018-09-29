"use strict";

/* global Script, Entities, print */

(function() {
    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var propertiesToEntities = EUs.propertiesToEntities;

    var req = new XMLHttpRequest();
    req.responseType = 'json';
    req.open("GET", "http://headache.hungry.com/~seth/junk/models.json", false);
    req.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
    req.send();
    if (req.status == 200) {
        var newEntityIDs = propertiesToEntities(req.response,
                                                { x: -110.3, y: -6.4356, z: -72.5 },
                                                { x: 0, y: 0, z: 0, w: 1 },
                                                false);
        for (var i = 0; i < newEntityIDs.length; i++) {
            var entityID = newEntityIDs[i];
            Entities.editEntity(entityID, { locked: true });
        }
    } else {
        print("Error loading data: " + req.status + " " + req.statusText + ", " + req.errorCode);
    }
})();
