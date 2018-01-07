"use strict";

/* global MyAvatar, Entities, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    // var destinationDomainName = "eschatology";
    // var destinationDomainName = "BEACH";
    var destinationDomainName = "CITY";
    var placeURL = "https://metaverse.highfidelity.com/api/v1/places/" + destinationDomainName;

    var request = new XMLHttpRequest();
    request.onreadystatechange = function() {
        if (request.readyState === request.DONE && request.status === 200) {
            var response = JSON.parse(request.responseText);
            var sphereID = Entities.addEntity({
                name: "Portal Sphere",
                lifetime: 120,
                type: "Sphere",
                color: { blue: 255, green: 255, red: 255 },
                position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, { x: 0, y: 0.2, z: -4 })),
                rotation: Quat.fromVec3Degrees({ x: 0, y: 0, z: 0 }),
                dimensions: { x: 1, y: 2, z: 1 },
                collisionless: true,
                userData: JSON.stringify({
                    grabbableKey: {
                        grabbable: true
                    },
                    ProceduralEntity: {
                        version: 2,
                        shaderUrl: Script.resolvePath("portal.fs"),
                        channels: [response.data.place.previews.lobby]
                        // channels: [response.data.place.previews.thumbnail]
                    }
                })
            });

            Entities.addEntity({
                name: "Portal Zone",
                dimensions: { x: 1, y: 2, z: 1 },
                script: Script.resolvePath("portalES.js"),
                shapeType: "box",
                type: "Zone",
                userData: JSON.stringify({
                    "teleportal-destination": "hifi://" + destinationDomainName + "/"
                }),
                parentID: sphereID,
                localPosition: { x: 0, y: 0, z: 0 },
                localRotation: { x: 0, y: 0, z: 0, w: 1 }
            });
        }
    };

    request.open('GET', placeURL);
    request.timeout = 10000;
    request.send();

}()); // END LOCAL_SCOPE
