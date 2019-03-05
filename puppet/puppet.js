"use strict";

/* global Vec3, Quat, MyAvatar, Entities, Script, Graphics */

(function() {
    var AppUi = Script.require('appUi');

    var ui;
    function startup() {
        ui = new AppUi({
            buttonName: "PUPPET",
            home: Script.resolvePath("puppet.qml"),
            onMessage: fromQml,
            // normalButton: "icons/tablet-icons/avatar-i.svg",
            // activeButton: "icons/tablet-icons/avatar-a.svg",
        });
    }
    startup();
    Script.scriptEnding.connect(function () {
        // Controller.mousePressEvent.disconnect(onMousePressEvent);
        // Controller.mouseReleaseEvent.disconnect(onMouseReleaseEvent);
        // Controller.mouseMoveEvent.disconnect(onMouseMoveEvent);
        // pages.clear();
        // killEngineInspectorView();
        // killCullInspectorView();
        // killEngineLODWindow();
    });


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

    var scale = 1.1;
    // var lifetime = 120;
    var lifetime = -1;

    var neckLength = scale * 0.05;
    var shoulderGap = scale * 0.1;
    var elbowGap = scale * 0.06;
    var handGap = scale * 0.05;
    var hipGap = scale * 0.07;
    var kneeGap = scale * 0.08;
    var ankleGap = scale * 0.06;
    var ankleMin = 0;
    var ankleMax = Math.PI / 4;

    var headSize = scale * 0.2;

    var bodyHeight = scale * 0.4;
    var bodyWidth = scale * 0.3;
    var bodyDepth = scale * 0.2;

    var upperArmThickness = scale * 0.05;
    var upperArmLength = scale * 0.2;

    var lowerArmThickness = scale * 0.05;
    var lowerArmLength = scale * 0.2;

    var handDiameter = scale * 0.06;

    var legLength = scale * 0.3;
    var legThickness = scale * 0.08;

    var shinLength = scale * 0.2;
    var shinThickness = scale * 0.06;

    var footLength = scale * 0.2;
    var footThickness = scale * 0.03;
    var footWidth = scale * 0.08;


    var puppetEntities = [];

    function rezPuppet() {

        // var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 1.0, z: -2}));
        var pos = MyAvatar.jointToWorldPoint({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Head"));
        // var rot = MyAvatar.jointToWorldDirection({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Head"));

        //
        // body
        //

        var bodyID = Entities.addEntity({
            name: "puppet body",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: bodyDepth, y: bodyHeight, z: bodyWidth },
            position: Vec3.sum(pos, { x: 0, y: scale * 0.0, z:0 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(bodyID);

        Entities.addAction("tractor", bodyID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("Spine"),
            tag: "puppet to spine spring"
        });


        //
        // head
        //

        var headID = Entities.addEntity({
            name: "puppet head",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: headSize, y: headSize, z: headSize },
            position: Vec3.sum(pos, { x: 0, y: bodyHeight / 2 + headSize / 2 + neckLength, z:0 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(headID);

        var noseID = Entities.addEntity({
            name: "puppet nose",
            type: "Box",
            color: { blue: 128, green: 100, red: 100 },
            dimensions: { x: headSize / 5, y: headSize / 5, z: headSize / 5 },
            localPosition: { x: headSize / 2 + headSize / 10, y: 0, z: 0 },
            dynamic: false,
            collisionless: true,
            collidesWith: "static,dynamic,kinematic",
            lifetime: lifetime,
            parentID: headID,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(noseID);

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

        Entities.addAction("tractor", headID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, -90, 0),
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("Head"),
            tag: "cone-twist test spring"
        });

        //
        // right upper arm
        //

        var rightUpperArmID = Entities.addEntity({
            name: "puppet right arm",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
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
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightUpperArmID);

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

        //
        // left upper arm
        //

        var leftUpperArmID = Entities.addEntity({
            name: "puppet left arm",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
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
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftUpperArmID);

        Entities.addAction("cone-twist", bodyID, {
            pivot: { x: 0, y: bodyHeight / 2 + upperArmThickness / 2, z: -bodyWidth / 2 - shoulderGap / 2 },
            axis: { x: 0, y: 0, z: -1 },
            otherEntityID: leftUpperArmID,
            otherPivot: { x: 0, y: 0, z: upperArmLength / 2 + shoulderGap / 2 },
            otherAxis: { x: 0, y: 0, z: -1 },
            swingSpan1: Math.PI / 2,
            swingSpan2: Math.PI / 2,
            twistSpan: 0,
            tag: "puppet left shoulder joint"
        });

        //
        // right lower arm
        //

        var rightLowerArmID = Entities.addEntity({
            name: "puppet right lower arm",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: bodyWidth / 2 + shoulderGap + upperArmLength + elbowGap + lowerArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightLowerArmID);

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

        //
        // left lower arm
        //

        var leftLowerArmID = Entities.addEntity({
            name: "puppet left lower arm",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: lowerArmThickness, y: lowerArmThickness, z: lowerArmLength },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap - lowerArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftLowerArmID);

        Entities.addAction("hinge", leftLowerArmID, {
            pivot: { x: 0, y: 0, z: lowerArmLength / 2 + elbowGap / 2 },
            axis: { x: 0, y: 1, z: 0 },
            otherEntityID: leftUpperArmID,
            otherPivot: { x: 0, y: 0, z: -upperArmLength / 2 - elbowGap / 2 },
            otherAxis: { x: 0, y: 1, z: 0 },
            low: 0,
            high: Math.PI / 2,
            tag: "puppet left elbow joint"
        });

        //
        // right hand
        //

        var rightHandID = Entities.addEntity({
            name: "puppet right hand",
            type: "Sphere",
            color: { blue: 20, green: 20, red: 200 },
            dimensions: { x: handDiameter, y: handDiameter, z: handDiameter },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: bodyWidth / 2 + shoulderGap + upperArmLength + elbowGap +
                                         lowerArmLength + handGap + handDiameter / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightHandID);

        Entities.addAction("cone-twist", rightHandID, {
            pivot: { x: 0, y: 0, z: -handDiameter / 2 - handGap / 2 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: rightLowerArmID,
            otherPivot: { x: 0, y:0, z: lowerArmLength / 2 + handGap / 2 },
            otherAxis: { x: 0, y: 0, z: 1 },
            swingSpan1: Math.PI / 2,
            swingSpan2: Math.PI / 2,
            twistSpan: Math.PI,
            tag: "puppet right wrist joint"
        });

        Entities.addAction("tractor", rightHandID, {
            targetRotation: { x: 0, y: 0, z: 0, w: 0 },
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightHand"),
            tag: "puppet to right-hand spring"
        });

        //
        // left hand
        //

        var leftHandID = Entities.addEntity({
            name: "puppet left hand",
            type: "Sphere",
            color: { blue: 20, green: 20, red: 200 },
            dimensions: { x: handDiameter, y: handDiameter, z: handDiameter },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap -
                                         lowerArmLength - handGap - handDiameter / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftHandID);

        Entities.addAction("cone-twist", leftHandID, {
            pivot: { x: 0, y: 0, z: handDiameter / 2 + handGap / 2 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: leftLowerArmID,
            otherPivot: { x: 0, y:0, z: -lowerArmLength / 2 - handGap / 2 },
            otherAxis: { x: 0, y: 0, z: 1 },
            swingSpan1: Math.PI / 2,
            swingSpan2: Math.PI / 2,
            twistSpan: Math.PI,
            tag: "puppet left wrist joint"
        });

        Entities.addAction("tractor", leftHandID, {
            targetRotation: { x: 0, y: 0, z: 0, w: 0 },
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftHand"),
            tag: "puppet to left-hand spring"
        });

        //
        // right leg
        //

        var rightLegID = Entities.addEntity({
            name: "puppet right arm",
            type: "Box",
            color: { blue: 20, green: 200, red: 20 },
            dimensions: { x: legThickness, y: legLength, z: legThickness },
            position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: bodyWidth / 2 - legThickness / 2 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightLegID);

        Entities.addAction("cone-twist", rightLegID, {
            pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
            axis: { x: 0, y: 1, z: 0 },
            otherEntityID: bodyID,
            otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: bodyWidth / 2 - legThickness / 2 },
            otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
            swingSpan1: Math.PI / 4,
            swingSpan2: Math.PI / 4,
            twistSpan: 0,
            tag: "puppet right hip joint"
        });

        //
        // left leg
        //

        var leftLegID = Entities.addEntity({
            name: "puppet left arm",
            type: "Box",
            color: { blue: 20, green: 200, red: 20 },
            dimensions: { x: legThickness, y: legLength, z: legThickness },
            position: Vec3.sum(pos, { x: 0, y: -bodyHeight / 2 - hipGap - legLength / 2, z: -bodyWidth / 2 + legThickness / 2 }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftLegID);

        Entities.addAction("cone-twist", leftLegID, {
            pivot: { x: 0, y: legLength / 2 + hipGap / 2, z: 0 },
            axis: { x: 0, y: 1, z: 0 },
            otherEntityID: bodyID,
            otherPivot: { x: 0, y: -bodyHeight / 2 - hipGap / 2, z: -bodyWidth / 2 + legThickness / 2 },
            otherAxis: Vec3.normalize({ x: -1, y: 1, z: 0 }),
            swingSpan1: Math.PI / 4,
            swingSpan2: Math.PI / 4,
            twistSpan: 0,
            tag: "puppet left hip joint"
        });

        //
        // right shin
        //

        var rightShinID = Entities.addEntity({
            name: "puppet right shin",
            type: "Box",
            color: { blue: 20, green: 200, red: 20 },
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
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightShinID);

        Entities.addAction("hinge", rightShinID, {
            pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: rightLegID,
            otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
            otherAxis: { x: 0, y: 0, z: 1 },
            low: 0,
            high: Math.PI / 2,
            tag: "puppet right knee joint"
        });


        //
        // left shin
        //

        var leftShinID = Entities.addEntity({
            name: "puppet left shin",
            type: "Box",
            color: { blue: 20, green: 200, red: 20 },
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
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftShinID);

        Entities.addAction("hinge", leftShinID, {
            pivot: { x: 0, y: shinLength / 2 + kneeGap / 2, z: 0 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: leftLegID,
            otherPivot: { x: 0, y: -legLength / 2 - kneeGap / 2, z: 0 },
            otherAxis: { x: 0, y: 0, z: 1 },
            low: 0,
            high: Math.PI / 2,
            tag: "puppet left knee joint"
        });

        //
        // right foot
        //

        var rightFootID = Entities.addEntity({
            name: "puppet right foot",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: footLength, y: footThickness, z: footWidth },
            position: Vec3.sum(pos, { x: -shinThickness / 2 + footLength / 2,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength -
                                         ankleGap - footThickness / 2,
                                      z: bodyWidth / 2 - legThickness / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(rightFootID);

        Entities.addAction("hinge", rightFootID, {
            pivot: { x: -footLength / 2 + shinThickness / 2, y: ankleGap / 2, z: 0 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: rightShinID,
            otherPivot: { x: 0, y: -shinLength / 2 - ankleGap / 2, z: 0 },
            otherAxis: { x: 0, y: 0, z: 1 },
            low: ankleMin,
            high: ankleMax,
            tag: "puppet right ankle joint"
        });

        Entities.addAction("tractor", rightFootID, {
            targetRotation: { x: 0, y: 0, z: 0, w: 0 },
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("RightFoot"),
            tag: "puppet to right-foot spring"
        });

        //
        // left foot
        //

        var leftFootID = Entities.addEntity({
            name: "puppet left foot",
            type: "Box",
            color: { blue: 128, green: 100, red: 20 },
            dimensions: { x: footLength, y: footThickness, z: footWidth },
            position: Vec3.sum(pos, { x: -shinThickness / 2 + footLength / 2,
                                      y: -bodyHeight / 2 - hipGap - legLength - kneeGap - shinLength -
                                         ankleGap - footThickness / 2,
                                      z: bodyWidth / 2 - legThickness / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.push(leftFootID);

        Entities.addAction("hinge", leftFootID, {
            pivot: { x: -footLength / 2 + shinThickness / 2, y: ankleGap / 2, z: 0 },
            axis: { x: 0, y: 0, z: 1 },
            otherEntityID: leftShinID,
            otherPivot: { x: 0, y: -shinLength / 2 - ankleGap / 2, z: 0 },
            otherAxis: { x: 0, y: 0, z: 1 },
            low: ankleMin,
            high: ankleMax,
            tag: "puppet left ankle joint"
        });

        Entities.addAction("tractor", leftFootID, {
            targetRotation: { x: 0, y: 0, z: 0, w: 0 },
            targetPosition: { x: 0, y: 0, z: 0 },
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftFoot"),
            tag: "puppet to left-foot spring"
        });
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
            // for (var m = 0; m < mesh.meshes.length; m++) { // XXX not mesh.meshes.length
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
                                    opacity: 0.2,
                                    defaultFallthrough: true
                                }
                            }),
                            localPosition: Vec3.ZERO,
                            localRotation: Quat.IDENTITY,
                            parentID: MyAvatar.sessionUUID,
                            priority: topMaterial.priority + 1,
                            parentMaterialName: m.toString()
                        });

                        puppetEntities.push(materialID);
                    }
                }
            }
        }
    }

    function deletePuppet() {
        puppetEntities.forEach(function (entityID) {
            Entities.deleteEntity(entityID);
        });
        puppetEntities = [];
    }

    function cleanup() {
        deletePuppet();
    }

    Script.scriptEnding.connect(cleanup);

}());
