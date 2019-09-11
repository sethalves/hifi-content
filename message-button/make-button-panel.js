
"use strict";

/* global Entities, Script, MyAvatar, Vec3, Quat */

(function() { // BEGIN LOCAL_SCOPE

    var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.1, z: -3}));

    // var buttons = [
    //     {
    //         text: "Day",
    //         channel: "Day-Night-Cycle",
    //         message: "{ \"method\":\"set-cycle-stage\", \"value\": 0 }"
    //     },
    //     {
    //         text: "Night",
    //         channel: "Day-Night-Cycle",
    //         message: "{ \"method\":\"set-cycle-stage\", \"value\": 4 }"
    //     },
    //     {
    //         text: "Enable Away",
    //         channel: "Hifi-Away-Enable",
    //         message: "enable"
    //     },
    //     {
    //         text: "Disable Away",
    //         channel: "Hifi-Away-Enable",
    //         message: "disable"
    //     }
    // ];


    var buttons = [
        {
            text: "1000",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 1000 }"
        },
        {
            text: "1200",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 1200 }"
        },
        {
            text: "1300",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 1300 }"
        },
        {
            text: "1400",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 1400 }"
        },
        {
            text: "1500",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 1500 }"
        },
        {
            text: "2000",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 2000 }"
        },
        {
            text: "2500",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 2500 }"
        },
        {
            text: "3000",
            channel: "mirror-control",
            message: "{ \"method\":\"set-resolution\", \"value\": 3000 }"
        }
    ];


    var margin = 0.015;
    var lineHeight = 0.12;
    var buttonSize = 0.3;
    var baseHeight = buttons.length * buttonSize + (2 * margin);
    var baseWidth = 1.06;
    var baseThickness = 0.2;
    var baseColor = { red: 105, green: 124, blue: 130 };
    var frameSize = 0.02;
    var frameColor = { red: 204, green: 229, blue: 255 };
    var frameDepth = baseThickness + 0.05;

    var lifetime = -1;
    // var lifetime = 30;


    var baseID = Entities.addEntity({
        type: "Box",
        dimensions: { x: baseWidth, y: baseHeight, z: baseThickness },
        grab: { grabbable: false },
        position: pos,
        rotation: MyAvatar.orientation,
        // registrationPoint: { x: 0.5, y: 0.5, z: 0.5 },
        color: baseColor,
        shape: "Cube",
        lifetime: lifetime,
        locked: false
    });

    // build frame around base
    Entities.addEntity({
        type: "Box",
        parentID: baseID,
        grab: { grabbable: false },
        localPosition: { x: (-baseWidth / 2) - (frameSize / 2), y: 0, z: 0},
        localDimensions: { x: frameSize, y: baseHeight + (2 * frameSize), z: frameDepth },
        localRotation: { x: 0, y: 0, z: 0, w: 1 },
        color: frameColor,
        shape: "Cube",
        lifetime: lifetime,
        locked: true
    });
    Entities.addEntity({
        type: "Box",
        parentID: baseID,
        grab: { grabbable: false },
        localPosition: { x: (baseWidth / 2) + (frameSize / 2), y: 0, z: 0},
        localDimensions: { x: frameSize, y: baseHeight + (2 * frameSize), z: frameDepth },
        localRotation: { x: 0, y: 0, z: 0, w: 1 },
        color: frameColor,
        shape: "Cube",
        lifetime: lifetime,
        locked: true
    });
    Entities.addEntity({
        type: "Box",
        parentID: baseID,
        grab: { grabbable: false },
        localPosition: { x: 0, y: (baseHeight / 2) + (frameSize / 2), z: 0},
        localDimensions: { x: baseWidth, y: frameSize, z: frameDepth },
        localRotation: { x: 0, y: 0, z: 0, w: 1 },
        color: frameColor,
        shape: "Cube",
        lifetime: lifetime,
        locked: true
    });
    Entities.addEntity({
        type: "Box",
        parentID: baseID,
        grab: { grabbable: false },
        localPosition: { x: 0, y: (-baseHeight / 2) - (frameSize / 2), z: 0},
        localDimensions: { x: baseWidth, y: frameSize, z: frameDepth },
        localRotation: { x: 0, y: 0, z: 0, w: 1 },
        color: frameColor,
        shape: "Cube",
        lifetime: lifetime,
        locked: true
    });




    for (var i = 0; i < buttons.length; i++) {
        var button = buttons[ i ];

        var y = (baseHeight / 2) - (i * buttonSize) - (buttonSize / 2) - margin;
        var buttonPosition = { x: (-baseWidth / 2) + margin + (buttonSize / 2), y: y, z: 0.1 };

        Entities.addEntity({
            type: "Sphere",
            parentID: baseID,
            grab: { grabbable: false },
            name: "Button: " + button.text,
            userData: JSON.stringify({
                messageButton: {
                    channel: button.channel,
                    message: button.message
                }
            }),
            localPosition: buttonPosition,
            localDimensions: { x: buttonSize - 0.1, y: buttonSize - 0.1, z: buttonSize - 0.1 },
            localRotation: { x: 0, y: 0, z: 0, w: 1 },
            script: Script.resolvePath("message-button.js"),
            color: { red: 0, green: 128, blue: 255 },
            lifetime: lifetime,
            locked: true
        });

        Entities.addEntity({
            name: "Button Label: " + button.text,
            type: "Text",
            parentID: baseID,
            grab: { grabbable: false },
            localPosition: { x: (-baseWidth / 2) + (2 * margin) + buttonSize, y: y, z: 0.14 },
            registrationPoint: { x: -1.0, y: 0.7, z: 0.5 },
            localDimensions: { x: 0.7, y: 0.2, z: 0.01 },
            localRotation: { x: 0, y: 0, z: 0, w: 1 },
            text: button.text,
            textColor: { red: 255, green: 255, blue: 255 },
            unlit: true,
            lineHeight: lineHeight,
            backgroundAlpha: 0,
            // leftMargin: 0.03,
            // topMargin: 0.03,
            lifetime: lifetime,
            locked: true
        });

        Entities.addEntity({
            type: "Shape",
            dimensions: { x: buttonSize * 0.8, y: buttonSize / 2, z: buttonSize * 0.8 },
            parentID: baseID,
            grab: { grabbable: false },
            localPosition: { x: buttonPosition.x, y: buttonPosition.y, z: 0.05 },
            localRotation: Quat.fromPitchYawRollDegrees(90, 0, 0),
            color: frameColor,
            shape: "Cylinder",
            lifetime: lifetime,
            locked: true
        });

    }


}()); // END LOCAL_SCOPE
