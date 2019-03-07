"use strict";

/* global Vec3, Quat, MyAvatar, Entities, Script, Graphics, Controller, Messages, getGrabPointSphereOffset */

Script.include("/~/system/libraries/controllers.js");

(function() {
    var AppUi = Script.require("appUi");

    var ui;

    function fromQml(message) {
        print("message from qml: " + JSON.stringify(message));
        if (message.method == "rez") {
            deletePuppet();
            fadeAvatar();
            rezPuppet();
        } else if (message.method == "derez") {
            deletePuppet();
        }
    }

    function jointToJointDistance(jointNameA, jointNameB) {
        return Vec3.distance(MyAvatar.jointToWorldPoint(Vec3.ZERO, MyAvatar.getJointIndex(jointNameA)),
                             MyAvatar.jointToWorldPoint(Vec3.ZERO, MyAvatar.getJointIndex(jointNameB)));
    }

    var scale = 1.1;
    // var lifetime = 120;
    var lifetime = 7000;
    var alpha = 1.0;

    var puppetEntities = {};


    function rezRightArm(pos, bodyID, bodyWidth, bodyHeight, shoulderGap, upperArmThickness, upperArmLength,
                         lowerArmThickness, lowerArmLength, elbowGap, swordLength, handDiameter, wristGap) {

        var grabPointOffset = getGrabPointSphereOffset(Controller.Standard.RightHand, false);

        //
        // right upper arm
        //

        var rightUpperArmID = Entities.addEntity({
            name: "puppet body right arm",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: upperArmThickness, y: upperArmThickness, z: upperArmLength },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 + upperArmThickness / 2,
                                      z: bodyWidth / 2 + shoulderGap + upperArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightUpperArm = rightUpperArmID;

        Entities.addAction("cone-twist", bodyID, {
            pivot: { x: 0, y: bodyHeight / 2 + upperArmThickness / 2, z: bodyWidth / 2 + shoulderGap / 2 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: rightUpperArmID,
            otherPivot: { x: 0, y: 0, z: -upperArmLength / 2 - shoulderGap / 2 },
            otherAxis: { x: 0, y: 0, z: 1 },
            swingSpan1: Math.PI / 2,
            swingSpan2: Math.PI / 2,
            twistSpan: 0,
            tag: "puppet right shoulder joint"
        });

        // Entities.addAction("tractor", rightUpperArmID, {
        //     targetRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
        //     angularTimeScale: 0.02,
        //     targetPosition: { x: 0, y: upperArmLength / 2, z: 0 },
        //     linearTimeScale: 0.02,
        //     otherID: MyAvatar.sessionUUID,
        //     otherJointIndex: MyAvatar.getJointIndex("RightArm"),
        //     tag: "puppet right upper spring"
        // });


        //
        // right lower arm
        //

        var rightLowerArmID = Entities.addEntity({
            name: "puppet body right lower arm",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: bodyWidth / 2 + shoulderGap + upperArmLength + elbowGap + lowerArmLength / 2
                                    }),
            dynamic: true,
            collisionless: true,
            // collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightLowerArm = rightLowerArmID;

        Entities.addAction("hinge", rightLowerArmID, {
            pivot: { x: 0, y: 0, z: -lowerArmLength / 2 - elbowGap / 2 },
            axis: { x: 0, y: 1, z: 0 },
            otherEntityID: rightUpperArmID,
            otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + elbowGap / 2 },
            otherAxis: { x: 0, y: 1, z: 0 },
            low: Math.PI / -2,
            high: 0,
            tag: "puppet right elbow joint"
        });

        // Entities.addAction("cone-twist", rightLowerArmID, {
        //     pivot: { x: 0, y: 0, z: -lowerArmLength / 2 - elbowGap / 2 },
        //     axis: { x: 0, y: 0, z: 1 },
        //     otherEntityID: rightUpperArmID,
        //     otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + elbowGap / 2 },
        //     otherAxis: { x: 0, y: 0, z: 1 },
        //     swingSpan1: 2 * Math.PI,
        //     swingSpan2: 2 * Math.PI,
        //     twistSpan: 2 * Math.PI,
        //     tag: "puppet right elbow joint"
        // });

        // Entities.addAction("tractor", rightLowerArmID, {
        //     targetRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
        //     angularTimeScale: 0.02,
        //     targetPosition: { x: 0, y: lowerArmLength / 2 + elbowGap, z: 0 },
        //     linearTimeScale: 0.02,
        //     otherID: MyAvatar.sessionUUID,
        //     otherJointIndex: MyAvatar.getJointIndex("RightForeArm"),
        //     tag: "puppet right forearm spring"
        // });


        //
        // right hand
        //

        var rightHandID = Entities.addEntity({
            name: "puppet right hand",
            type: "Box",
            color: { red: 200, green: 20, blue: 20 },
            dimensions: { x: handDiameter / 4, y: swordLength, z: handDiameter / 4 },
            registrationPoint: { x: 0.5, y: 0.0, z: 0.5 },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: bodyWidth / 2 + shoulderGap + upperArmLength + elbowGap +
                                         lowerArmLength + wristGap + handDiameter / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            script: Script.resolvePath("hit-detector.js"),
            alpha: alpha,
            ignorePickIntersection: true,
            grab: { grabbable: false }
        });
        puppetEntities.rightHand = rightHandID;

        Entities.addAction("cone-twist", rightHandID, {
            pivot: { x: 0, y: 0, z: -handDiameter / 2 - wristGap / 2 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: rightLowerArmID,
            otherPivot: { x: 0, y:0, z: lowerArmLength / 2 + wristGap / 2 },
            otherAxis: { x: 0, y: 0, z: 1 },
            swingSpan1: Math.PI / 2,
            swingSpan2: Math.PI / 2,
            twistSpan: Math.PI,
            tag: "puppet right wrist joint"
        });

        Entities.addAction("tractor", rightHandID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, -70),
            targetPosition: grabPointOffset,
            linearTimeScale: 0.01,
            angularTimeScale: 0.01,
            maximumActiveDistance: 0.07,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightHand"),
            tag: "puppet to right-hand spring"
        });

        //
        // grab-point indicator on hand
        //

        var rightHandGrabPointID = Entities.addEntity({
            name: "puppet right hand grab point",
            type: "Sphere",
            color: { red: 220, green: 160, blue: 0 },
            dimensions: { x: handDiameter / 4, y: handDiameter / 4, z: handDiameter / 4 },
            localPosition: grabPointOffset,
            dynamic: true,
            collisionless: true,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            parentID: MyAvatar.sessionUUID,
            parentJointIndex: MyAvatar.getJointIndex("RightHand"),
            ignorePickIntersection: true,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightHandGrabPoint = rightHandGrabPointID;


        //
        // grab-point indicator on sword
        //

        var rightHandGrabSwordID = Entities.addEntity({
            name: "puppet right hand grab sword point",
            type: "Sphere",
            color: { red: 220, green: 160, blue: 0 },
            dimensions: { x: handDiameter / 4, y: handDiameter / 4, z: handDiameter / 4 },
            localPosition: { x: 0, y: 0, z: 0 },
            dynamic: true,
            collisionless: true,
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            parentID: rightHandID,
            grab: { grabbable: false }
        });
        puppetEntities.rightHandGrabSword = rightHandGrabSwordID;
    }


    function rezPuppet() {

        var neckLength = scale * 0.05;
        var shoulderGap = scale * 0.1;
        var elbowGap = scale * 0.06;
        var wristGap = scale * 0.05;
        var hipGap = scale * 0.07;
        var kneeGap = scale * 0.08;
        var ankleGap = scale * 0.06;
        // var ankleMin = 0;
        // var ankleMax = Math.PI / 4;

        var headSize = scale * 0.2;

        // var spineOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Spine"));
        // var hipsOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Hips"));
        // var neckOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Neck"));

        var bodyHeight = jointToJointDistance("Hips", "Neck"); // scale * 0.4;
        var bodyWidth = scale * 0.3; // jointToJointDistance("LeftShoulder", "RightShoulder")
        var bodyDepth = scale * 0.2;

        var upperArmThickness = scale * 0.05;
        var upperArmLength = jointToJointDistance("LeftShoulder", "LeftForeArm") - (elbowGap / 2) - (shoulderGap / 2);

        var lowerArmThickness = scale * 0.05;
        var lowerArmLength = jointToJointDistance("LeftForeArm", "LeftHand") - (elbowGap / 2) - (wristGap / 2);

        var handDiameter = scale * 0.24;

        var legLength = jointToJointDistance("RightUpLeg", "RightLeg") - (hipGap / 2) - (kneeGap / 2);
        var legThickness = scale * 0.08;

        var shinLength = jointToJointDistance("RightLeg", "RightFoot") - (kneeGap / 2) - (ankleGap / 2);
        var shinThickness = scale * 0.06;

        var footLength = scale * 0.2;
        var footThickness = scale * 0.03;
        var footWidth = scale * 0.08;

        var shieldOffset = lowerArmThickness * 3;
        var swordLength = 1.1;



        // var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 1.0, z: -2}));
        var pos = MyAvatar.jointToWorldPoint({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Head"));
        // var rot = MyAvatar.jointToWorldDirection({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Head"));

        //
        // body
        //

        var bodyID = Entities.addEntity({
            name: "puppet body main",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: bodyDepth, y: bodyHeight, z: bodyWidth },
            position: Vec3.sum(pos, { x: 0, y: scale * 0.0, z:0 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.body = bodyID;

        // var bodyOffset = Vec3.multiply(Vec3.sum(hipsOffset, neckOffset), 0.5);
        // var bodyOffsetFromSpine = Vec3.subtract(bodyOffset, spineOffset);

        Entities.addAction("tractor", bodyID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
            // targetPosition: bodyOffsetFromSpine,
            // linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("Spine"),
            tag: "puppet to spine spring"
        });


        //
        // head
        //

        var headLocalPosition = Vec3.sum(pos, { x: 0, y: bodyHeight / 2 + headSize / 2 + neckLength, z:0 });
        var headID = Entities.addEntity({
            name: "puppet body head",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: headSize, y: headSize, z: headSize },
            position: headLocalPosition,
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.head = headID;

        // var noseID = Entities.addEntity({
        //     name: "puppet body nose",
        //     type: "Box",
        //     color: { red: 100, green: 100, blue: 128 },
        //     dimensions: { x: headSize / 5, y: headSize / 5, z: headSize / 5 },
        //     localPosition: { x: headSize / 2 + headSize / 10, y: 0, z: 0 },
        //     dynamic: false,
        //     collisionless: true,
        //     collidesWith: "static,dynamic,kinematic",
        //     lifetime: lifetime,
        //     alpha: alpha,
        //     parentID: headID,
        //     grab: { grabbable: true, grabKinematic: false }
        // });
        // puppetEntities.nose = noseID;

        Entities.addAction("cone-twist", headID, {
            pivot: { x: 0, y: -headSize / 2 - neckLength / 2, z: 0 },
            axis: { x: 0, y: 1, z: 0 },
            otherEntityID: bodyID,
            otherPivot: { x: 0, y: bodyHeight / 2 + neckLength / 2, z: 0 },
            otherAxis: { x: 0, y: 1, z: 0 },
            swingSpan1: Math.PI / 4,
            swingSpan2: Math.PI / 4,
            twistSpan: Math.PI / 2,
            tag: "puppet neck joint"
        });

        // var hipsToNeck = Vec3.subtract(MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Neck")),
        //                                MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Hips")));

        Entities.addAction("tractor", headID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
            targetPosition: Vec3.ZERO,
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("Head"),
            tag: "cone-twist test spring"
        });

        //
        // left upper arm
        //

        var leftUpperArmID = Entities.addEntity({
            name: "puppet body left arm",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: upperArmThickness, y: upperArmThickness, z: upperArmLength },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 + upperArmThickness / 2,
                                      z: -bodyWidth / 2 - shoulderGap - upperArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.leftUpperArm = leftUpperArmID;

        // Entities.addAction("cone-twist", bodyID, {
        //     pivot: { x: 0, y: bodyHeight / 2 + upperArmThickness / 2, z: -bodyWidth / 2 - shoulderGap / 2 },
        //     axis: { x: 0, y: 0, z: -1 },
        //     otherEntityID: leftUpperArmID,
        //     otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + shoulderGap / 2 },
        //     otherAxis: { x: 0, y: 0, z: -1 },
        //     swingSpan1: Math.PI / 2,
        //     swingSpan2: Math.PI / 2,
        //     twistSpan: 0,
        //     tag: "puppet left shoulder joint"
        // });

        Entities.addAction("tractor", leftUpperArmID, {
            targetRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: upperArmLength / 2, z: 0 },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftArm"),
            tag: "puppet left upper spring"
        });


        //
        // left lower arm
        //

        // var leftLowerArmID = Entities.addEntity({
        //     name: "puppet body left lower arm",
        //     type: "Box",
        //     color: { red: 20, green: 100, blue: 128 },
        //     dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
        //     position: Vec3.sum(pos, { x: 0,
        //                               y: bodyHeight / 2 - upperArmThickness / 2,
        //                               z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap - lowerArmLength / 2
        //                             }),
        //     dynamic: true,
        //     collisionless: false,
        //     collidesWith: "static,dynamic,kinematic",
        //     gravity: { x: 0, y: 0, z: 0 },
        //     lifetime: lifetime,
        //     alpha: alpha,
        //     ignorePickIntersection: true,
        //     grab: { grabbable: true, grabKinematic: false }
        // });

        var leftLowerArmID = Entities.addEntity({
            name: "puppet shield",
            type: "Shape",
            color: { red: 212, green: 0, blue: 240 },
            dimensions: { x: lowerArmLength * 2, y: handDiameter / 2, z: lowerArmLength * 2 },
            shape: "Cylinder",
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap - lowerArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            ignorePickIntersection: true,
            grab: { grabbable: true, grabKinematic: false }
        });

        puppetEntities.leftLowerArm = leftLowerArmID;

        // Entities.addAction("hinge", leftLowerArmID, {
        //     pivot: { x: 0, y: 0, z: lowerArmLength / 2 + elbowGap / 2 },
        //     axis: { x: 0, y: 1, z: 0 },
        //     otherEntityID: leftUpperArmID,
        //     otherPivot: { x: 0, y: 0, z: -upperArmLength / 2 - elbowGap / 2 },
        //     otherAxis: { x: 0, y: 1, z: 0 },
        //     low: 0,
        //     high: Math.PI / 2,
        //     tag: "puppet left elbow joint"
        // });

        // Entities.addAction("tractor", leftLowerArmID, {
        //     targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
        //     angularTimeScale: 0.02,
        //     otherID: MyAvatar.sessionUUID,
        //     otherJointIndex: MyAvatar.getJointIndex("LeftForeArm"),
        //     tag: "puppet left forearm spring"
        // });

        Entities.addAction("tractor", leftLowerArmID, {
            targetRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: lowerArmLength / 2 + elbowGap, z: -shieldOffset },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftForeArm"),
            tag: "puppet left forearm spring"
        });


        //
        // left hand
        //

        // var leftHandID = Entities.addEntity({
        //     name: "puppet left hand",
        //     type: "Sphere",
        //     color: { red: 200, green: 20, blue: 20 },
        //     dimensions: { x: handDiameter / 2, y: handDiameter / 2, z: handDiameter / 2 },
        //     position: Vec3.sum(pos, { x: 0,
        //                               y: bodyHeight / 2 - upperArmThickness / 2,
        //                               z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap -
        //                                  lowerArmLength - wristGap - handDiameter / 2
        //                             }),
        //     dynamic: true,
        //     collisionless: false,
        //     collidesWith: "static,dynamic,kinematic",
        //     gravity: { x: 0, y: 0, z: 0 },
        //     lifetime: lifetime,
        //     alpha: alpha,
        //     grab: { grabbable: true, grabKinematic: false }
        // });
        // puppetEntities.leftHand = leftHandID;

        // Entities.addAction("cone-twist", leftHandID, {
        //     pivot: { x: 0, y: 0, z: handDiameter / 2 + wristGap / 2 },
        //     axis: { x: 0, y: 0, z: -1 },
        //     otherEntityID: leftLowerArmID,
        //     otherPivot: { x: 0, y:0, z: -lowerArmLength / 2 - wristGap / 2 },
        //     otherAxis: { x: 0, y: 0, z: -1 },
        //     swingSpan1: 2 * Math.PI,
        //     swingSpan2: 2 * Math.PI,
        //     twistSpan: 2 * Math.PI,
        //     tag: "puppet left wrist joint"
        // });

        // Entities.addAction("tractor", leftHandID, {
        //     // targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
        //     targetPosition: { x: 0, y: 0, z: 0 },
        //     linearTimeScale: 0.02,
        //     // angularTimeScale: 0.02,
        //     otherID: MyAvatar.sessionUUID,
        //     otherJointIndex: MyAvatar.getJointIndex("LeftHand"),
        //     tag: "puppet to left-hand spring"
        // });

        //
        // right leg
        //


        rezRightArm(pos, bodyID, bodyWidth, bodyHeight, shoulderGap, upperArmThickness, upperArmLength,
                    lowerArmThickness, lowerArmLength, elbowGap, swordLength, handDiameter, wristGap);


        var rightLegID = Entities.addEntity({
            name: "puppet body right leg",
            type: "Box",
            color: { red: 20, green: 200, blue: 20 },
            dimensions: { x: legThickness, y: legLength, z: legThickness },
            position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: bodyWidth / 2 - legThickness / 2 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightLeg = rightLegID;

        // Entities.addAction("cone-twist", rightLegID, {
        //     pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
        //     axis: { x: 0, y: 1, z: 0 },
        //     otherEntityID: bodyID,
        //     otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: bodyWidth / 2 - legThickness / 2 },
        //     otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
        //     swingSpan1: Math.PI / 4,
        //     swingSpan2: Math.PI / 4,
        //     twistSpan: 0,
        //     tag: "puppet right hip joint"
        // });

        Entities.addAction("tractor", rightLegID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: legLength / 2 + kneeGap, z: 0 },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightUpLeg"),
            tag: "puppet right leg spring"
        });


        //
        // left leg
        //

        var leftLegID = Entities.addEntity({
            name: "puppet body left arm",
            type: "Box",
            color: { red: 20, green: 200, blue: 20 },
            dimensions: { x: legThickness, y: legLength, z: legThickness },
            position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: -bodyWidth / 2 + legThickness / 2 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.leftLeg = leftLegID;

        // Entities.addAction("cone-twist", leftLegID, {
        //     pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
        //     axis: { x: 0, y: 1, z: 0 },
        //     otherEntityID: bodyID,
        //     otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: -bodyWidth / 2 + legThickness / 2 },
        //     otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
        //     swingSpan1: Math.PI / 4,
        //     swingSpan2: Math.PI / 4,
        //     twistSpan: 0,
        //     tag: "puppet left hip joint"
        // });

        Entities.addAction("tractor", leftLegID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: legLength / 2 + kneeGap, z: 0 },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftUpLeg"),
            tag: "puppet left leg spring"
        });

        //
        // right shin
        //

        var rightShinID = Entities.addEntity({
            name: "puppet body right shin",
            type: "Box",
            color: { red: 20, green: 200, blue: 20 },
            dimensions: { x: shinThickness, y: shinLength, z: shinThickness },
            position: Vec3.sum(pos, { x: 0,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength / 2,
                                      z: bodyWidth / 2 - legThickness / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightShin = rightShinID;

        // Entities.addAction("hinge", rightShinID, {
        //     pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
        //     axis: { x: 0, y: 0, z: 1 },
        //     otherEntityID: rightLegID,
        //     otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
        //     otherAxis: { x: 0, y: 0, z: 1 },
        //     low: 0,
        //     high: Math.PI / 2,
        //     tag: "puppet right knee joint"
        // });

        Entities.addAction("tractor", rightShinID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: shinLength / 2 + kneeGap, z: 0 },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightLeg"),
            tag: "puppet right shin spring"
        });


        //
        // left shin
        //

        var leftShinID = Entities.addEntity({
            name: "puppet body left shin",
            type: "Box",
            color: { red: 20, green: 200, blue: 20 },
            dimensions: { x: shinThickness, y: shinLength, z: shinThickness },
            position: Vec3.sum(pos, { x: 0,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength / 2,
                                      z: -bodyWidth / 2 + legThickness / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.leftShin = leftShinID;

        // Entities.addAction("hinge", leftShinID, {
        //     pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
        //     axis: { x: 0, y: 0, z: 1 },
        //     otherEntityID: leftLegID,
        //     otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
        //     otherAxis: { x: 0, y: 0, z: 1 },
        //     low: 0,
        //     high: Math.PI / 2,
        //     tag: "puppet left knee joint"
        // });

        Entities.addAction("tractor", leftShinID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, 0),
            angularTimeScale: 0.02,
            targetPosition: { x: 0, y: shinLength / 2 + kneeGap, z: 0 },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftLeg"),
            tag: "puppet left shin spring"
        });

        //
        // right foot
        //

        var rightFootID = Entities.addEntity({
            name: "puppet body right foot",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: footLength, y: footThickness, z: footWidth },
            position: Vec3.sum(pos, { x: -shinThickness / 2 + footLength / 2,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength -
                                         ankleGap - footThickness / 2,
                                      z: bodyWidth / 2 - legThickness / 2
                                    }),
            dynamic: true,
            collisionless: true,
            // collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.rightFoot = rightFootID;

        // Entities.addAction("hinge", rightFootID, {
        //     pivot: { x: -footLength / 2 + shinThickness / 2, y: ankleGap / 2, z: 0 },
        //     axis: { x: 0, y: 0, z: 1 },
        //     otherEntityID: rightShinID,
        //     otherPivot: { x: 0, y: -shinLength / 2 - ankleGap / 2, z: 0 },
        //     otherAxis: { x: 0, y: 0, z: 1 },
        //     low: ankleMin,
        //     high: ankleMax,
        //     tag: "puppet right ankle joint"
        // });

        Entities.addAction("tractor", rightFootID, {
            targetRotation: Quat.fromPitchYawRollDegrees(90, 0, 90),
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightToeBase"),
            tag: "puppet to right-foot spring"
        });

        //
        // left foot
        //

        var leftFootID = Entities.addEntity({
            name: "puppet body left foot",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: footLength, y: footThickness, z: footWidth },
            position: Vec3.sum(pos, { x: -shinThickness / 2 + footLength / 2,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength -
                                         ankleGap - footThickness / 2,
                                      z: bodyWidth / 2 - legThickness / 2
                                    }),
            dynamic: true,
            collisionless: true,
            // collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.leftFoot = leftFootID;

        // Entities.addAction("hinge", leftFootID, {
        //     pivot: { x: -footLength / 2 + shinThickness / 2, y: ankleGap / 2, z: 0 },
        //     axis: { x: 0, y: 0, z: 1 },
        //     otherEntityID: leftShinID,
        //     otherPivot: { x: 0, y: -shinLength / 2 - ankleGap / 2, z: 0 },
        //     otherAxis: { x: 0, y: 0, z: 1 },
        //     low: ankleMin,
        //     high: ankleMax,
        //     tag: "puppet left ankle joint"
        // });

        Entities.addAction("tractor", leftFootID, {
            targetRotation: Quat.fromPitchYawRollDegrees(90, 0, 90),
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftToeBase"),
            tag: "puppet to left-foot spring"
        });
    }

    function handleHit(data) {
        // var myID = data.myID;
        var otherID = data.otherID;
        // var hitType = data.collisionInfo.type;
        // var penetration = data.collisionInfo.penetration;
        var contactPoint = data.collisionInfo.contactPoint;
        var velocityChange = data.collisionInfo.velocityChange;

        // "collisionInfo": {
        //     "type": 2,
        //     "idA": "{10bfae34-02f8-4eca-a1cf-c5f8c07b9905}",
        //     "idB": "{80188ac5-ebfa-4a0c-b95e-e84ea1d1516f}",
        //     "penetration": {
        //         "x": 0.006713929120451212,
        //         "y": 0.0016198069788515568,
        //         "z": -0.00239554513245821
        //     },
        //     "contactPoint": {
        //         "x": 80.54314422607422,
        //         "y": 1.6683143377304077,
        //         "z": 75.14118957519531
        //     },
        //     "velocityChange": {
        //         "x": 0.08970536291599274,
        //         "y": -0.03307801112532616,
        //         "z": -0.031879980117082596
        //     }
        // }

        var otherProps = Entities.getEntityProperties(otherID, ["name"]);
        var otherName = otherProps.name;
        if (otherName && otherName.substring(0, 12) == "puppet body ") {
            // var hitSize = Vec3.length(velocityChange) * 2;

            var hitSize = Math.log(Vec3.length(velocityChange) + 1.0);

            Entities.addEntity({
                name: "puppet hit indicator",
                type: "Sphere",
                color: { red: 220, green: 140, blue: 0 },
                dimensions: { x: hitSize, y: hitSize, z: hitSize },
                position: contactPoint,
                dynamic: false,
                collisionless: true,
                gravity: { x: 0, y: 0, z: 0 },
                lifetime: 5,
                alpha: 0.8,
                grab: { grabbable: false }
            });
        }
    }

    function getTopMaterial(multiMaterial) {
        // For non-models: multiMaterial[0] will be the top material
        // For models, multiMaterial[0] is the base material, and multiMaterial[1] is the highest priority applied material
        if (multiMaterial.length > 1) {
            if (multiMaterial[1].priority > multiMaterial[0].priority) {
                return multiMaterial[1];
            }
        }

        return multiMaterial[0];
    }

    function fadeAvatar() {
        var mesh = Graphics.getModel(MyAvatar.sessionUUID);
        if (mesh) {
            var materials = mesh.materialLayers;
            for (var m in materials) {
                if (materials.hasOwnProperty(m) && parseInt(m.toString()) == m) {
                    var multiMaterial = materials[m];
                    if (multiMaterial) {
                        var topMaterial = getTopMaterial(multiMaterial);
                        var materialID = Entities.addEntity({
                            name: "puppet avatar-fade " + m,
                            type: "Material",
                            materialURL: "materialData",
                            materialData: JSON.stringify({
                                materialVersion: 1,
                                materials: {
                                    model: "hifi_pbr",
                                    opacity: 0.0,
                                    defaultFallthrough: true
                                }
                            }),
                            localPosition: Vec3.ZERO,
                            localRotation: Quat.IDENTITY,
                            parentID: MyAvatar.sessionUUID,
                            priority: topMaterial.priority + 1,
                            parentMaterialName: m.toString()
                        });

                        puppetEntities["material-" + m] = materialID;
                    }
                }
            }
        }
    }

    function deletePuppet() {
        for (var key in puppetEntities) {
            if (puppetEntities.hasOwnProperty(key)) {
                var entityID = puppetEntities[key];
                Entities.deleteEntity(entityID);
            }
        }

        puppetEntities = {};
    }

    function handleMessages(channel, message, sender) {
        if (sender !== MyAvatar.sessionUUID) {
            return;
        }
        if (channel !== "Puppet-Sword-Fight") {
            return;
        }
        var data;
        try {
            data = JSON.parse(message);
        } catch (e) {
            print("WARNING: error parsing 'Puppet-Sword-Fight' message: " + message);
            return;
        }

        var method = data.method;
        if (method == "hit") {
            handleHit(data);
        }
    }

    function cleanup() {
        Messages.unsubscribe("Puppet-Sword-Fight");
        Messages.messageReceived.disconnect(handleMessages);
        deletePuppet();
    }

    function startup() {
        ui = new AppUi({
            buttonName: "PUPPET",
            home: Script.resolvePath("puppet.qml"),
            onMessage: fromQml,
            // normalButton: "icons/tablet-icons/avatar-i.svg",
            // activeButton: "icons/tablet-icons/avatar-a.svg",
        });

        Script.scriptEnding.connect(cleanup);
        Messages.messageReceived.connect(handleMessages);
        Messages.subscribe("Puppet-Sword-Fight");
    }

    startup();

}());
