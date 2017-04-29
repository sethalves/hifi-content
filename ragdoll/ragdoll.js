
/* global Vec3, MyAvatar, Entities */


var scale = 1.6;
var lifetime = 120;
var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 1.0, z: -2}));

var neckLength = scale * 0.05;
var shoulderGap = scale * 0.06;
var elbowGap = scale * 0.06;
var hipGap = scale * 0.07;
var kneeGap = scale * 0.08;

var headSize = scale * 0.2;

var bodyHeight = scale * 0.4;
var bodyWidth = scale * 0.3;
var bodyDepth = scale * 0.2;

var upperArmThickness = scale * 0.05;
var upperArmLength = scale * 0.2;

var lowerArmThickness = scale * 0.05;
var lowerArmLength = scale * 0.2;

var legLength = scale * 0.3;
var legThickness = scale * 0.08;

var shinLength = scale * 0.2;
var shinThickness = scale * 0.06;


//
// body
//

var bodyID = Entities.addEntity({
    name: "ragdoll body",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: bodyDepth, y: bodyHeight, z: bodyWidth },
    position: Vec3.sum(pos, { x: 0, y: scale * 0.0, z:0 }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

//
// head
//

var headID = Entities.addEntity({
    name: "ragdoll head",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: headSize, y: headSize, z: headSize },
    position: Vec3.sum(pos, { x: 0, y: bodyHeight / 2 + headSize / 2 + neckLength, z:0 }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

var noseID = Entities.addEntity({
    name: "ragdoll nose",
    type: "Box",
    color: { blue: 128, green: 100, red: 100 },
    dimensions: { x: headSize / 5, y: headSize / 5, z: headSize / 5 },
    localPosition: { x: headSize / 2 + headSize / 10, y: 0, z: 0 },
    dynamic: false,
    collisionless: true,
    lifetime: lifetime,
    parentID: headID,
    userData: "{ \"grabbableKey\": { \"grabbable\": false } }"
});

Entities.addAction("cone-twist", headID, {
    pivot: { x: 0, y: -headSize / 2 - neckLength / 2, z: 0 },
    axis: { x: 0, y: 1, z: 0 },
    otherEntityID: bodyID,
    otherPivot: { x: 0, y: bodyHeight / 2 + neckLength / 2, z: 0 },
    otherAxis: { x: 0, y: 1, z: 0 },
    swingSpan1: Math.PI / 4,
    swingSpan2: Math.PI / 4,
    twistSpan: Math.PI / 2,
    tag: "ragdoll neck joint"
});

//
// right upper arm
//

var rightUpperArmID = Entities.addEntity({
    name: "ragdoll right arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: upperArmThickness, y: upperArmThickness, z: upperArmLength },
    position: Vec3.sum(pos, { x: 0,
                              y: bodyHeight / 2 - upperArmThickness / 2,
                              z: bodyWidth / 2 + shoulderGap + upperArmLength / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("cone-twist", bodyID, {
    pivot: { x: 0, y: bodyHeight / 2 - upperArmThickness / 2, z: bodyWidth / 2 + shoulderGap / 2 },
    axis: { x: 0, y: 0, z: 1 },
    otherEntityID: rightUpperArmID,
    otherPivot: { x: 0, y: 0, z: -upperArmLength / 2 - shoulderGap / 2 },
    otherAxis: { x: 0, y: 0, z: 1 },
    swingSpan1: Math.PI / 2,
    swingSpan2: Math.PI / 2,
    twistSpan: 0,
    tag: "ragdoll right shoulder joint"
});

//
// left upper arm
//

var leftUpperArmID = Entities.addEntity({
    name: "ragdoll left arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: upperArmThickness, y: upperArmThickness, z: upperArmLength },
    position: Vec3.sum(pos, { x: 0,
                              y: bodyHeight / 2 - upperArmThickness / 2,
                              z: -bodyWidth / 2 - shoulderGap - upperArmLength / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("cone-twist", bodyID, {
    pivot: { x: 0, y: bodyHeight / 2 - upperArmThickness / 2, z: -bodyWidth / 2 - shoulderGap / 2 },
    axis: { x: 0, y: 0, z: -1 },
    otherEntityID: leftUpperArmID,
    otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + shoulderGap / 2 },
    otherAxis: { x: 0, y: 0, z: -1 },
    swingSpan1: Math.PI / 2,
    swingSpan2: Math.PI / 2,
    twistSpan: 0,
    tag: "ragdoll left shoulder joint"
});

//
// right lower arm
//

var rightLowerArmID = Entities.addEntity({
    name: "ragdoll right lower arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
    position: Vec3.sum(pos, { x: 0,
                              y: bodyHeight / 2 - upperArmThickness / 2,
                              z: bodyWidth / 2 + shoulderGap + upperArmLength + elbowGap + lowerArmLength / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("hinge", rightLowerArmID, {
    pivot: { x: 0, y: 0, z: -lowerArmLength / 2 - elbowGap / 2 },
    axis: { x: 0, y: 1, z: 0 },
    otherEntityID: rightUpperArmID,
    otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + elbowGap / 2 },
    otherAxis: { x: 0, y: 1, z: 0 },
    low: Math.PI / -2,
    high: 0,
    tag: "ragdoll right elbow joint"
});

//
// left lower arm
//

var leftLowerArmID = Entities.addEntity({
    name: "ragdoll left lower arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
    position: Vec3.sum(pos, { x: 0,
                              y: bodyHeight / 2 - upperArmThickness / 2,
                              z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap - lowerArmLength / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("hinge", leftLowerArmID, {
    pivot: { x: 0, y: 0, z: lowerArmLength / 2 + elbowGap / 2 },
    axis: { x: 0, y: 1, z: 0 },
    otherEntityID: leftUpperArmID,
    otherPivot: { x: 0, y: 0, z: -upperArmLength / 2 - elbowGap / 2 },
    otherAxis: { x: 0, y: 1, z: 0 },
    low: 0,
    high: Math.PI / 2,
    tag: "ragdoll left elbow joint"
});

//
// right leg
//

var rightLegID = Entities.addEntity({
    name: "ragdoll right arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: legThickness, y: legLength, z: legThickness },
    position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: bodyWidth / 2 - legThickness / 2 }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("cone-twist", rightLegID, {
    pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
    axis: { x: 0, y: 1, z: 0 },
    otherEntityID: bodyID,
    otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: bodyWidth / 2 - legThickness / 2 },
    otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
    swingSpan1: Math.PI / 4,
    swingSpan2: Math.PI / 4,
    twistSpan: 0,
    tag: "ragdoll right hip joint"
});

//
// left leg
//

var leftLegID = Entities.addEntity({
    name: "ragdoll left arm",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: legThickness, y: legLength, z: legThickness },
    position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: -bodyWidth / 2 + legThickness / 2 }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("cone-twist", leftLegID, {
    pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
    axis: { x: 0, y: 1, z: 0 },
    otherEntityID: bodyID,
    otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: -bodyWidth / 2 + legThickness / 2 },
    otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
    swingSpan1: Math.PI / 4,
    swingSpan2: Math.PI / 4,
    twistSpan: 0,
    tag: "ragdoll left hip joint"
});

//
// right shin
//

var rightShinID = Entities.addEntity({
    name: "ragdoll right shin",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: shinThickness, y: shinLength, z: shinThickness },
    position: Vec3.sum(pos, { x: 0,
                              y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength / 2,
                              z: bodyWidth / 2 - legThickness / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("hinge", rightShinID, {
    pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
    axis: { x: 0, y: 0, z: 1 },
    otherEntityID: rightLegID,
    otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
    otherAxis: { x: 0, y: 0, z: 1 },
    low: 0,
    high: Math.PI / 2,
    tag: "ragdoll right knee joint"
});


//
// left shin
//

var leftShinID = Entities.addEntity({
    name: "ragdoll left shin",
    type: "Box",
    color: { blue: 128, green: 100, red: 20 },
    dimensions: { x: shinThickness, y: shinLength, z: shinThickness },
    position: Vec3.sum(pos, { x: 0,
                              y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength / 2,
                              z: -bodyWidth / 2 + legThickness / 2
                            }),
    dynamic: true,
    collisionless: false,
    gravity: { x: 0, y: 0, z: 0 },
    lifetime: lifetime,
    userData: "{ \"grabbableKey\": { \"grabbable\": true, \"kinematic\": false } }"
});

Entities.addAction("hinge", leftShinID, {
    pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
    axis: { x: 0, y: 0, z: 1 },
    otherEntityID: leftLegID,
    otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
    otherAxis: { x: 0, y: 0, z: 1 },
    low: 0,
    high: Math.PI / 2,
    tag: "ragdoll left knee joint"
});
