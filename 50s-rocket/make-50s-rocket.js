//
//
//

var avatarFront = Quat.getFront(Camera.getOrientation());
avatarFront.y = 0.0;
var center = Vec3.sum(MyAvatar.position, Vec3.multiply(15, avatarFront));
center = Vec3.sum(center, { x: 0, y: 10, z: 0 });

Entities.addEntity({
    name: '50s rocket',
    type: 'Model',
    modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket.obj',
    compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-collision-hull.obj',
    position: center,
    script: 'http://headache.hungry.com/~seth/hifi/50s-rocket.js',
    dynamic: true,
    gravity: { x: 0, y: -5.0, z: 0 },
    velocity: { x: 0, y: 0.1, z: 0 }, // to make it fall

    // put center where it is in openscad
    // registrationPoint: { x: 0.5,
    //                      y: 0.0049751, // model height is 20.1, this is (/ 0.1 20.1)
    //                      z: 0.5 },
});

Entities.addEntity({
    name: '50s rocket door',
    type: 'Model',
    modelURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door.obj',
    // compoundShapeURL: 'http://headache.hungry.com/~seth/hifi/50s-rocket-door-collision-hull.obj',
    position: center,
    dynamic: false,
    gravity: { x: 0, y: 0, z: 0 },
    // registrationPoint: { x: 0.5, y: 0.0, z: 0.0 },
    angularDamping: { x: 0.0, y: 0.0, z: 0.0 },
});
