
/* globals Vec3, MyAvatar, Script, Entities, Quat */


(function () {

    Script.include("/~/system/libraries/controllers.js");

    function jointToJointDistance(jointNameA, jointNameB) {
        return Vec3.distance(MyAvatar.jointToWorldPoint(Vec3.ZERO, MyAvatar.getJointIndex(jointNameA)),
                             MyAvatar.jointToWorldPoint(Vec3.ZERO, MyAvatar.getJointIndex(jointNameB)));
    }


    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var scale = 1.1;
    var alpha = 0.3;
    var lifetime = 7000;

    var neckLength = scale * 0.05;
    // var shoulderGap = scale * 0.1;
    var elbowGap = scale * 0.06;
    var wristGap = scale * 0.05;

    var headSize = scale * 0.2;

    // var spineOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Spine"));
    // var hipsOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Hips"));
    // var neckOffset = MyAvatar.getAbsoluteJointTranslationInObjectFrame(MyAvatar.getJointIndex("Neck"));

    var bodyHeight = jointToJointDistance("Hips", "Neck"); // scale * 0.4;
    var bodyWidth = scale * 0.3; // jointToJointDistance("LeftShoulder", "RightShoulder")
    var bodyDepth = scale * 0.2;

    // var upperArmThickness = scale * 0.05;
    // var upperArmLength = jointToJointDistance("LeftShoulder", "LeftForeArm") - (elbowGap / 2) - (shoulderGap / 2);

    var lowerArmThickness = scale * 0.05;
    var lowerArmLength = jointToJointDistance("LeftForeArm", "LeftHand") - (elbowGap / 2) - (wristGap / 2);

    var handDiameter = scale * 0.24;

    var shieldOffset = lowerArmThickness * 3;
    var swordLength = 1.1;



    var puppetEntities = {};

    function rezSword(hand, hiltID) {
        var props = Entities.getEntityProperties(hiltID, ["position", "rotation",
                                                          "grab.equippableLeftPosition",
                                                          "grab.equippableRightPosition",
                                                          "grab.equippableLeftRotation",
                                                          "grab.equippableRightRotation"]);
        // var grabProps = props.grab;
        // var targetPosition = (hand == LEFT_HAND ? grabProps.equippableLeftPosition : grabProps.equippableRightPosition);
        // var targetRotation = (hand == LEFT_HAND ? grabProps.equippableLeftRotation : grabProps.equippableRightRotation);

        var handJointIndex = MyAvatar.getJointIndex(hand == LEFT_HAND ? "LeftHand" : "RightHand");
        var targetPosition = MyAvatar.worldToJointPoint(props.position, handJointIndex);
        var targetRotation = MyAvatar.worldToJointRotation(props.rotation, handJointIndex);

        var swordID = Entities.addEntity({
            name: "Sword",
            type: "Shape",
            shape: "Cylinder",
            color: { red: 200, green: 20, blue: 20 },
            dimensions: { x: handDiameter / 4, y: swordLength, z: handDiameter / 4 },
            registrationPoint: { x: 0.5, y: 0.0, z: 0.5 },
            position: props.position,
            rotation: props.rotation,
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
        puppetEntities.sword = swordID;

        Entities.addAction("tractor", swordID, {
            // targetRotation: Quat.fromPitchYawRollDegrees(0, 0, hand == RIGHT_HAND ? -70 : 70),
            targetPosition: targetPosition,
            targetRotation: targetRotation,
            linearTimeScale: 0.6,
            angularTimeScale: 0.2,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: handJointIndex,
            tag: "sword spring"
        });
    }

    function rezShield(hand, hiltID) {
        var armJointIndex = MyAvatar.getJointIndex(hand == LEFT_HAND ? "LeftForeArm" : "RightForeArm");
        var handJointIndex = MyAvatar.getJointIndex(hand == LEFT_HAND ? "LeftHand" : "RightHand");
        var targetPosition = { x: 0, y: lowerArmLength / 2 + elbowGap, z: -shieldOffset };
        var targetRotation = Quat.fromPitchYawRollDegrees(-90, 0, 0);

        var shieldID = Entities.addEntity({
            name: "Shield",
            type: "Shape",
            shape: "Cylinder",
            color: { red: 212, green: 0, blue: 240 },
            dimensions: { x: lowerArmLength * 2.5, y: handDiameter / 4, z: lowerArmLength * 3.0 },
            position: MyAvatar.jointToWorldPoint(targetPosition, armJointIndex),
            // rotation: Quat.multiply(targetRotation, MyAvatar.jointToWorldDirection(handJointIndex)),
            rotation: MyAvatar.jointToWorldRotation(targetRotation, handJointIndex),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: 1.0,
            ignorePickIntersection: true,
            grab: { grabbable: true, grabKinematic: false }
        });

        puppetEntities.shield = shieldID;

        Entities.addAction("tractor", shieldID, {
            targetPosition: targetPosition,
            linearTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: armJointIndex,
            tag: "shield linear forearm spring"
        });

        Entities.addAction("tractor", shieldID, {
            targetRotation: targetRotation,
            angularTimeScale: 0.02,
            otherID: MyAvatar.sessionUUID,
            otherJointIndex: handJointIndex,
            tag: "shield rotational forearm spring"
        });

        return shieldID;
    }

    function rezTargets(hiltID) {
        //
        // body
        //

        var bodyID = Entities.addEntity({
            name: "puppet body main",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: bodyDepth, y: bodyHeight, z: bodyWidth },
            position: MyAvatar.jointToWorldPoint({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Spine")),
            dynamic: true,
            collisionless: false,
            collidesWith: "static,dynamic,kinematic",
            gravity: { x: 0, y: 0, z: 0 },
            lifetime: lifetime,
            alpha: alpha,
            grab: { grabbable: true, grabKinematic: false }
        });
        puppetEntities.body = bodyID;

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

        var headID = Entities.addEntity({
            name: "puppet body head",
            type: "Box",
            color: { red: 20, green: 100, blue: 128 },
            dimensions: { x: headSize, y: headSize, z: headSize },
            position: MyAvatar.jointToWorldPoint({x: 0, y: 0, z: 0}, MyAvatar.getJointIndex("Head")),
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
            tag: "puppet head tractor"
        });
    }


    function rezHealthBar(hand, shieldID) {
    }


    function cleanUp() {
        for (var key in puppetEntities) {
            if (puppetEntities.hasOwnProperty(key)) {
                var entityID = puppetEntities[key];
                Entities.deleteEntity(entityID);
            }
        }

        puppetEntities = {};
    }


    this.preload = function (entityID) {
        print("QQQQ preload");
        this.entityID = entityID;
    };

    this.unload = function() {
        print("QQQQ unload");
        cleanUp();
    };

    this.startEquip = function (id, params) {
        this.hand = params[0] == "left" ? 0 : 1;
        this.equipperID = params[1];
        print("QQQQ start equip");
        rezSword(this.hand, this.entityID);
        var shieldID = rezShield(this.hand == LEFT_HAND ? RIGHT_HAND : LEFT_HAND, this.entityID);
        rezTargets(this.entityID);
        rezHealthBar(this.hand, shieldID);
    };

    this.releaseEquip = function (id, params) {
        this.hand = params[0] == "left" ? 0 : 1;
        this.equipperID = params[1];
        print("QQQQ stop equip");
        cleanUp();
    };

});
