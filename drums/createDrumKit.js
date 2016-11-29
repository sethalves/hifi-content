"use strict";

var SPRING_ENTITY_SCRIPT_URL = "https://hifi-content.s3.amazonaws.com/lincoln/ballSpring.js";

function getPosition(pos) {
    return Vec3.sum(MyAvatar.position, pos)
};

var position = {
    "x": 0.99825286865234375,
    "y": 0.03036003112792969,
    "z": 0.2906951904296875
};

var props = {
    "clientOnly": 0,
    "collisionless": 1,
    "created": "2016-10-12T00:17:10Z",
    "description": "Drum Set Base",
    "dimensions": {
        "x": 3.0173230171203613,
        "y": 1.8752377033233643,
        "z": 1.8151965141296387
    },
    "ignoreForCollisions": 1,
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/drums.fbx",
    "name": "Drum Set",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(position),
    "queryAACube": {
        "scale": 3.9894478321075439,
        "x": -1.014651894569397,
        "y": -1.9084447622299194,
        "z": -1.694339394569397
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "static-mesh",
    "type": "Model"
};

var drumKit = Entities.addEntity(props);

var pos1 = {
    "x": 1.8542060852050781,
    "y": 0.7531764984130859,
    "z": 0.4654388427734375
};

var prop1 = {
    "clientOnly": 0,
    "created": "2016-10-12T17:34:49Z",
    "description": "Drum Set Cymbal 2",
    "dimensions": {
        "x": 0.51723790168762207,
        "y": 0.14577460289001465,
        "z": 0.52338039875030518
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/cymbal2-head.fbx",
    "name": "RX7-CRASH",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos1),
    "queryAACube": {
        "scale": 0.75014150142669678,
        "x": 1.4445016384124756,
        "y": 0.45307451486587524,
        "z": 0.076146900653839111
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID1 = Entities.addEntity(prop1);

Entities.addAction("spring", springID1, {
    targetPosition: (prop1.position),
    linearTimeScale: 0.09,
    targetRotation: prop1.rotation,
    angularTimeScale: 0.001
});

var pos2 = {
    "x": 0.29206466674804688,
    "y": 0.8225555555555,
    "z": 0.4069671630859375
};

var prop2 = {
    "clientOnly": 0,
    "created": "2016-10-12T21:37:18Z",
    "description": "Drum Set T3",
    "dimensions": {
        "x": 0.65370982885360718,
        "y": 0.31387364864349365,
        "z": 0.690590500831604
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/cymbal3-head.fbx",
    "name": "RX17-CRASH",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos2),
    "queryAACube": {
        "scale": 1.0013833045959473,
        "x": -0.22229504585266113,
        "y": 0.41759014129638672,
        "z": -0.068112611770629883
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL,
    "dynamic": true
};

var springID2 = Entities.addEntity(prop2);

Entities.addAction("spring", springID2, {
    targetPosition: prop2.position,
    linearTimeScale: 0.09,
    targetRotation: prop2.rotation,
    angularTimeScale: 0.001
});

var pos3 = {
    "x": 0,
    "y": 0.4952939033508301,
    "z": 0
};

var prop3 = {
    "clientOnly": 0,
    "created": "2016-10-12T21:37:18Z",
    "description": "Drum Set T4",
    "dimensions": {
        "x": 0.7999345064163208,
        "y": 0.37546050548553467,
        "z": 0.82227444648742676
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/cymbal4-head.fbx",
    "name": "RIDE01",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos3),
    "queryAACube": {
        "scale": 1.2070629596710205,
        "x": -0.60353147983551025,
        "y": -0.023029923439025879,
        "z": -0.60353147983551025
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID3 = Entities.addEntity(prop3);

Entities.addAction("spring", springID3, {
    targetPosition: getPosition(pos3),
    linearTimeScale: 0.09,
    targetRotation: prop3.rotation,
    angularTimeScale: 0.001
});

var pos4 = {
    "x": 0.30172348022460938,
    "y": -0.095150555555555,
    "z": 0.13091278076171875
};

var prop4 = {
    "clientOnly": 0,
    "created": "2016-10-12T21:35:07Z",
    "description": "Drum Set B4",
    "dimensions": {
        "x": 0.64289271831512451,
        "y": 0.58302426338195801,
        "z": 0.6204296350479126
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/drum4-head.fbx",
    "name": "TOM3",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos4),
    "queryAACube": {
        "scale": 1.0668463706970215,
        "x": -0.24898791313171387,
        "y": -0.53342318534851074,
        "z": -0.39070773124694824
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID4 = Entities.addEntity(prop4);

Entities.addAction("spring", springID4, {
    targetPosition: prop4.position,
    linearTimeScale: 0.09,
    targetRotation: prop4.rotation,
    angularTimeScale: 0.001
});

var pos5 = {
    "x": 0.7304840087890625,
    "y": 0.16594390869140625,
    "z": 0.71282958984375
};

var prop5 = {
    "clientOnly": 0,
    "created": "2016-10-12T21:30:45Z",
    "description": "Drum Set B3",
    "dimensions": {
        "x": 0.41699486970901489,
        "y": 0.43548750877380371,
        "z": 0.48801636695861816
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/drum3-head.fbx",
    "name": "TOM4",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos5),
    "queryAACube": {
        "scale": 0.77568942308425903,
        "x": 0.32066664099693298,
        "y": -0.16211047768592834,
        "z": 0.33303388953208923
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID5 = Entities.addEntity(prop5);

Entities.addAction("spring", springID5, {
    targetPosition: prop5.position,
    linearTimeScale: 0.09,
    targetRotation: prop5.rotation,
    angularTimeScale: 0.001
});

var pos6 = {
    "x": 1.8945198059082031,
    "y": 0.4881258487701416,
    "z": 0.08203887939453125
};

var prop6 = {
    "description": "Drum Set T1",
    "dimensions": {
        "x": 0.44338709115982056,
        "y": 0.20397853851318359,
        "z": 0.44607818126678467
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/cymbal1-head.fbx",
    "name": "HH",
    "position": getPosition(pos6),
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID6 = Entities.addEntity(prop6);

Entities.addAction("spring", springID6, {
    targetPosition: prop6.position,
    linearTimeScale: 0.09,
    targetRotation: prop6.rotation,
    angularTimeScale: 0.001
});

var pos7 = {
    "x": 1.6030502319335938,
    "y": 0.062939453125,
    "z": 0.45587921142578125
};

var prop7 = {
    "clientOnly": 0,
    "created": "2016-10-12T21:35:07Z",
    "description": "Drum Set B1",
    "dimensions": {
        "x": 0.52639341354370117,
        "y": 0.36949896812438965,
        "z": 0.54592406749725342
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/drum1-head.fbx",
    "name": "SNARE01",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos7),
    "queryAACube": {
        "scale": 0.84359508752822876,
        "x": 1.1625339984893799,
        "y": -0.28986170887947083,
        "z": 0.01715967059135437
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID7 = Entities.addEntity(prop7);

Entities.addAction("spring", springID7, {
    targetPosition: prop7.position,
    linearTimeScale: 0.09,
    targetRotation: prop7.rotation,
    angularTimeScale: 0.001
});

var pos8 = {
    "x": 1.132354736328125,
    "y": 0.14058785438537598,
    "z": 0.81217193603515625
};

var prop8 = {
    "clientOnly": 0,
    "created": "2016-10-12T17:50:06Z",
    "description": "Drum Set B2",
    "dimensions": {
        "x": 0.34498164057731628,
        "y": 0.37092894315719604,
        "z": 0.39761841297149658
    },
    "locked": false,
    "modelURL": "https://hifi-content.s3.amazonaws.com/lincoln/drum/drum2-head.fbx",
    "name": "TOM2",
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(pos8),
    "queryAACube": {
        "scale": 0.64397281408309937,
        "x": 0.8205420970916748,
        "y": -0.098056763410568237,
        "z": 0.39829710125923157
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "shapeType": "simple-hull",
    "type": "Model",
    "dynamic": true,
    "visible": false,
    "script": SPRING_ENTITY_SCRIPT_URL
};

var springID8 = Entities.addEntity(prop8);

Entities.addAction("spring", springID8, {
    targetPosition: prop8.position,
    linearTimeScale: 0.09,
    targetRotation: prop8.rotation,
    angularTimeScale: 0.001
});

var boxPosition = {
    "x": 1.0913276672363281,
    "y": 0.40435791015625,
    "z": 0.13832855224609375
}

// Create EnterEntity box to attach drum sticks
var boxProperties = {
    "clientOnly": 0,
    "collisionless": 1,
    "color": {
        "blue": 0,
        "green": 0,
        "red": 255
    },
    "created": "2016-10-21T22:02:10Z",
    "description": "Drum Box",
    "dimensions": {
        "x": 0.91828072071075439,
        "y": 2.5950355529785156,
        "z": 1.5061792135238647
    },
    "id": "{3df0ca82-9edc-42b8-89b4-d54bf8dcd286}",
    "ignoreForCollisions": 1,
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    "position": getPosition(boxPosition),
    "queryAACube": {
        "scale": 3.1378376483917236,
        "x": -0.47759115695953369,
        "y": -1.1645609140396118,
        "z": -1.4305902719497681
    },
    "rotation": {
        "w": 1,
        "x": -1.52587890625e-05,
        "y": -1.52587890625e-05,
        "z": -1.52587890625e-05
    },
    "script": "https://hifi-content.s3.amazonaws.com/lincoln/drumEntityScript.js",
    "shape": "Cube",
    "type": "Box",
    "visible": 0
};

var drumBox = Entities.addEntity(boxProperties);

Script.stop();
