//
//
//

var SHOW_TOOL_BAR = true;

if (SHOW_TOOL_BAR) {
    var HIFI_PUBLIC_BUCKET = "http://s3.amazonaws.com/hifi-public/";
    Script.include(HIFI_PUBLIC_BUCKET + "scripts/libraries/toolBars.js");

    var BUTTON_SIZE = 32;
    var PADDING = 3;

    var toolBar = new ToolBar(0, 0, ToolBar.VERTICAL, "highfidelity.attachedEntities.toolbar", function(screenSize) {
        return {
            x: (BUTTON_SIZE + PADDING),
            y: (screenSize.y / 2 - BUTTON_SIZE * 2 + PADDING)
        };
    });
    var makeRocketButton = this.toolBar.addOverlay("image", {
        width: BUTTON_SIZE,
        height: BUTTON_SIZE,
        imageURL: HIFI_PUBLIC_BUCKET + "images/close.png",
        color: {
            red: 255,
            green: 255,
            blue: 255
        },
        alpha: 1
    });
}


function mousePressEvent(event) {
    var clickedOverlay = Overlays.getOverlayAtPoint({
        x: event.x,
        y: event.y
    });

    if (clickedOverlay == makeRocketButton) {
        makeRocket();
    }
}


function makeRocket() {
    var avatarFront = Quat.getFront(Camera.getOrientation());
    avatarFront.y = 0.0;
    var center = Vec3.sum(Vec3.sum(MyAvatar.position, Vec3.multiply(20, avatarFront)), { x: 0, y: 5, z: 0 });

    this.rocketID = Entities.addEntity({
        name: '50s rocket',
        type: 'Model',
        modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket.obj',
        compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-collision-hull.obj',
        collisionsWillMove: false,
        position: center,
        rotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
        script: 'http://headache.hungry.com/~seth/hifi/50s-rocket.js',
        dynamic: true,
        gravity: { x: 0, y: -1.0, z: 0 },
        velocity: { x: 0, y: -0.5, z: 0 }, // to make it fall
        // density: 8000,

        // put center where it is in openscad
        // registrationPoint: { x: 0.5,
        //                      y: 0.0049751, // model height is 20.1, this is (/ 0.1 20.1)
        //                      z: 0.5 },


        userData: JSON.stringify({
            "grabbableKey":{"grabbable":false}, "soundKey":{"url":"http://headache.hungry.com/~seth/hifi/sound/clock-ticking-3.wav","volume":0.4,"loop":true,"playbackGap":0,"playbackGapRange":0}
        }),
    });

    // see 50s-rocket.js -- Some of these values are copied out of edit.js after this.maintainDoor has been run.
    this.doorID = Entities.addEntity({
        name: '50s rocket door',
        type: 'Model',
        modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.obj',
        compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door-collision-hull.obj',
        dynamic: false,
        gravity: { x: 0, y: 0, z: 0 },
        angularDamping: { x: 0.0, y: 0.0, z: 0.0 },
        parentID: this.rocketID,
        parentJointIndex: -1,
        // collidesWith: "static,dynamic,kinematic,myAvatar,otherAvatar",
        collidesWith: "",
        script: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.js',
        registrationPoint: {
            "x": 0.5,
            "y": 0,
            "z": 0.076923079788684845
        },
        localPosition: {
            "x": 0.58713227510452271,
            "y": 0,
            "z": 3.7070074081420898
        },
        localRotation: {
            "w": 0.99688720703125,
            "x": 0,
            "y": 0.078446626663208008,
            "z": 0
        },
        userData: "{\"grabbableKey\":{\"wantsTrigger\":true}}"
    });
}

function scriptEnding() {
    if (SHOW_TOOL_BAR) {
        toolBar.cleanup();
    }
}

if (SHOW_TOOL_BAR) {
    Controller.mousePressEvent.connect(mousePressEvent);
}
Script.scriptEnding.connect(scriptEnding);
