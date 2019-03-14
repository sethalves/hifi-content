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
            // fadeAvatar();
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
    var alpha = 0.3;

    var puppetEntities = {};


    function rezRightArm(pos, bodyID, bodyWidth, bodyHeight, shoulderGap, upperArmThickness, upperArmLength,
                         lowerArmThickness, lowerArmLength, elbowGap, swordLength, handDiameter, wristGap) {

        var grabPointOffset = getGrabPointSphereOffset(Controller.Standard.RightHand, false);

        //
        // right hand
        //

        var swordID = Entities.addEntity({
            name: "puppet right hand",
            type: "Shape",
            shape: "Cylinder",
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
            alpha: 1.0,
            ignorePickIntersection: true,
            grab: { grabbable: false }
        });
        puppetEntities.rightHand = swordID;


        Entities.addAction("tractor", swordID, {
            targetRotation: Quat.fromPitchYawRollDegrees(0, 0, -70),
            targetPosition: grabPointOffset,
            linearTimeScale: 0.6,
            angularTimeScale: 0.6,
            // maximumActiveDistance: 0.07,
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
            parentID: swordID,
            grab: { grabbable: false }
        });
        puppetEntities.rightHandGrabSword = rightHandGrabSwordID;
    }


    function rezPuppet() {

        var neckLength = scale * 0.05;
        var shoulderGap = scale * 0.1;
        var elbowGap = scale * 0.06;
        var wristGap = scale * 0.05;

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
            targetPosition: Vec3.ZERO,
            linearTimeScale: 0.02,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("Head"),
            tag: "cone-twist test spring"
        });


        var shieldID = Entities.addEntity({
            name: "puppet shield",
            type: "Shape",
            shape: "Cylinder",
            color: { red: 212, green: 0, blue: 240 },
            dimensions: { x: lowerArmLength * 2.5, y: handDiameter / 4, z: lowerArmLength * 2.5 },
            position: Vec3.sum(pos, { x: 0,
                                      y: bodyHeight / 2 - upperArmThickness / 2,
                                      z: -bodyWidth / 2 - shoulderGap - upperArmLength - elbowGap - lowerArmLength / 2
                                    }),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: 1.0,
            ignorePickIntersection: true,
            grab: { grabbable: true, grabKinematic: false }
        });

        puppetEntities.leftLowerArm = shieldID;

        Entities.addAction("tractor", shieldID, {
            targetPosition: { x: 0, y: lowerArmLength / 2 + elbowGap, z: -shieldOffset },
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftForeArm"),
            tag: "puppet left forearm spring"
        });

        Entities.addAction("tractor", shieldID, {
            targetRotation: Quat.fromPitchYawRollDegrees(-90, 0, 0),
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: MyAvatar.getJointIndex("LeftHand"),
            tag: "puppet left forearm spring"
        });

        rezRightArm(pos, bodyID, bodyWidth, bodyHeight, shoulderGap, upperArmThickness, upperArmLength,
                    lowerArmThickness, lowerArmLength, elbowGap, swordLength, handDiameter, wristGap);
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

            var hitSize = Math.log(Vec3.length(velocityChange) + 1.0) / 4.0;

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
