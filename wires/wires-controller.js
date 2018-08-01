
"use strict";

/* global Messages, print */



// var array = [];
// function listKeys(string, object) {
//     if (string === "listKeys" || string === "array" || string === "buffer" || string === "i") {
//         return;
//     }

//     if (typeof(object) !== "object" || object === null) {
//         array.push(string + " " + typeof(object));
//         return;
//     }

//     var keys = Object.keys(object);
//     for (var i = 0; i < keys.length; ++i) {
//         if (string === "") {
//             listKeys(keys[i], object[keys[i]]);
//         } else if (keys[i] !== "parent") {
//             listKeys(string + "." + keys[i], object[keys[i]]);
//         }
//     }
// }

// listKeys("", this);
// array.sort();

// var buffer = "\n======= JS API list =======";
// for (var i = 0; i < array.length; ++i) {
//     buffer += "\n" + array[i];
// }
// buffer += "\n========= API END =========\n";

// print(buffer);


(function() {
    // var SCAN_RANGE = 4.0; // meters radius

    var self = this;

    self.preload = function (entityID) {
        self.entityID = entityID;
    };

    // var findComponentsAndWires = function () {
    //     var allEntities = Entities.findEntities(MyAvatar.position, SCAN_RANGE);
    // }

    Messages.subscribe('Hifi-Object-Manipulation');
    Messages.messageReceived.connect(function(channel, message, sender) {
        if (channel != 'Hifi-Object-Manipulation') {
            return;
        }
        try {
            var data = JSON.parse(message);
            print("Hifi-Object-Manipulation -- action=" + data.action + " id=" + data.grabbedEntity);
        } catch (e) {
            print("WARNING: wires-controller.js -- error parsing Hifi-Object-Manipulation message: " + message);
        }
    });

}); // END LOCAL_SCOPE
