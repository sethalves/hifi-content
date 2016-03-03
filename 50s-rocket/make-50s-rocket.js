//
//
//

var avatarFront = Quat.getFront(Camera.getOrientation());
avatarFront.y = 0.0;
var center = Vec3.sum(MyAvatar.position, Vec3.multiply(15, avatarFront));
center = Vec3.sum(center, { x: 0, y: 14, z: 0 });

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
    Entities.addEntity({
        name: '50s rocket',
        type: 'Model',
        modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket.obj',
        compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-collision-hull.obj',
        position: center,
        script: 'http://headache.hungry.com/~seth/hifi/50s-rocket.js',
        dynamic: true,
        gravity: { x: 0, y: -5.0, z: 0 },
        velocity: { x: 0, y: 0.5, z: 0 }, // to make it fall

        // put center where it is in openscad
        // registrationPoint: { x: 0.5,
        //                      y: 0.0049751, // model height is 20.1, this is (/ 0.1 20.1)
        //                      z: 0.5 },
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
