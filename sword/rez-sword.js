
/* globals Vec3, MyAvatar, Script, Entities */


Entities.addEntity({
    name: "Sword Hilt",
    type: "Shape",
    shape: "Cylinder",
    color: { red: 200, green: 20, blue: 20 },
    dimensions: { x: 0.07, y: 0.1, z: 0.07 },
    // registrationPoint: { x: 0.5, y: 0.0, z: 0.5 },
    position: Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.4, z: -1})),
    dynamic: false,
    collisionless: true,
    gravity: { x: 0, y: 0, z: 0 },
    script: Script.resolvePath("sword.js"),
    alpha: 1.0,
    grab: {
        grabbable: true,
        grabKinematic: true,
        grabFollowsController: true,
        triggerable: false,
        equippable: true,
        grabDelegateToParent: false,
        equippableLeftPosition: {
            x: -0.06589794903993607,
            y: 0.14683279395103455,
            z: 0.030722394585609436
        },
        equippableLeftRotation: {
            x: -0.5422598719596863,
            y: 0.4769817590713501,
            z: 0.5084458589553833,
            w: 0.46889448165893555
        },
        equippableRightPosition:    {
            x: 0.019753381609916687,
            y: 0.1004236489534378,
            z: 0.05624334514141083
        },
        equippableRightRotation: {
            x: 0.37679100036621094,
            y: 0.4304722547531128,
            z: 0.5785152912139893,
            w: -0.581384003162384
        }
    }
});
