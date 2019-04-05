
/* global Script, Entities, Vec3, Quat, MyAvatar */

var bezelID = Entities.addEntity({
    type: "Model",
    position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.2, z: -1.5})),
    dimensions: { x: 0.2, y: 0.02, z: 0.2 },
    grab: {
        grabbable: true,
        grabDelegateToParent: false
    },
    dynamic: true,
    collisionless: false,
    shapeType: "box",
    gravity: { x: 0, y: -1, z: 0 },
    name: "github throttling status bezel",
    modelURL: Script.resolvePath("bezel.obj")
});

Entities.addEntity({
    type: "Text",
    name: "github throttling status text",
    localPosition: { x: 0, y: 0, z: 0 },
    localRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
    text: "",
    textAlpha: 1,
    textColor: { red: 255, green: 255, blue: 255 },
    backgroundAlpha: 1,
    backgroundColor: { red: 0, green: 0, blue: 0 },
    lineHeight: 0.021,
    billboardMode: "none",
    dimensions: { x: 0.18, y: 0.18 },
    visible: true,
    grab: {
        grabbable: false,
        grabDelegateToParent: true
    },
    ignoreRayIntersection: true,
    drawInFront: false,
    parentID: bezelID,
    serverScripts: Script.resolvePath("status-page.js")
});
