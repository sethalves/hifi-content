
/* globals Vec3, MyAvatar, Script, Entities, Quat, Messages, SoundCache */


(function () {

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var addEntityAuto = EUs.addEntityAuto;

    var soundPainBody = SoundCache.getSound(Script.resolvePath("painBody.wav"));
    var soundPainHead = SoundCache.getSound(Script.resolvePath("painHead.wav"));
    var soundHitShield = SoundCache.getSound(Script.resolvePath("shieldHit.wav"));
    var soundSwordHit = SoundCache.getSound(Script.resolvePath("swordHit.wav"));
    var injector;
    var audioVolume = 0.06;

    var hitVelocityToHealthRatio = 80.0;

    var messagesEnabled = false;

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

    var health = 100;

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

        addEntityAuto({
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
        }, function (swordID) {
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
        });
    }

    function rezShield(hand, hiltID, doneThunk) {
        var armJointIndex = MyAvatar.getJointIndex(hand == LEFT_HAND ? "LeftForeArm" : "RightForeArm");
        var handJointIndex = MyAvatar.getJointIndex(hand == LEFT_HAND ? "LeftHand" : "RightHand");
        var targetPosition = { x: 0, y: lowerArmLength / 2 + elbowGap, z: -shieldOffset };
        var targetRotation = Quat.fromPitchYawRollDegrees(-90, 0, 0);

        addEntityAuto({
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
            grab: { grabbable: false, grabKinematic: false }
        }, function (shieldID) {
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
            doneThunk(shieldID);
        });
    }

    function rezTargets(hiltID) {
        //
        // body
        //

        addEntityAuto({
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
        }, function (bodyID) {
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

            addEntityAuto({
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
            }, function (headID) {
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
            });
        });
    }


    function getHealthBarDimensions() {
        var shieldProps = Entities.getEntityProperties(puppetEntities.shield, ["dimensions"]);
        var shieldDimensions = shieldProps.dimensions;

        var barLength = (health / 100.0) * (shieldDimensions.z - 0.1);
        return { x: 0.1, y: shieldDimensions.y + 0.05, z: barLength };
    }


    function rezHealthBar(hand, shieldID) {
        addEntityAuto({
            name: "sword fight health bar",
            type: "Box",
            color: { red: 220, green: 0, blue: 0 },
            dimensions: getHealthBarDimensions(),
            dynamic: false,
            collisionless: true,
            alpha: 0.8,
            grab: { grabbable: false },
            parentID: shieldID,
            parentJointIndex: -1,
            localPosition: { x: 0, y: 0, z: 0 },
            localRotation: { x: 0, y: 0, z: 0, w: 1 },
            localAngularVelocity: { x: 0, y: 0, z: 0 },
            localVelocity: { x: 0, y: 0, z: 0 }
        }, function (healthBarID) {
            puppetEntities.healthbar = healthBarID;
            // XXX why...?
            Entities.editEntity(healthBarID, {
                localPosition: { x: 0, y: 0, z: 0 },
                localRotation: { x: 0, y: 0, z: 0, w: 1 },
                localAngularVelocity: { x: 0, y: 0, z: 0 },
                localVelocity: { x: 0, y: 0, z: 0 }
            });
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


        var injectorOptions;
        if (otherID == puppetEntities.head) {
            injectorOptions = { position: contactPoint, volume: audioVolume };
            injector = Audio.playSound(soundPainHead, injectorOptions);
        } else if (otherID == puppetEntities.body) {
            injectorOptions = { position: contactPoint, volume: audioVolume };
            injector = Audio.playSound(soundPainBody, injectorOptions);
        } else if (otherID == puppetEntities.shield) {
            injectorOptions = { position: contactPoint, volume: audioVolume };
            injector = Audio.playSound(soundHitShield, injectorOptions);
        } else {
            injectorOptions = { position: contactPoint, volume: audioVolume };
            injector = Audio.playSound(soundSwordHit, injectorOptions);
        }

        if (otherID == puppetEntities.head || otherID == puppetEntities.body) {
            // var hitSize = Vec3.length(velocityChange) * 2;
            var hitSize = Math.log(Vec3.length(velocityChange) + 1.0) / 3.0;

            health -= (Vec3.length(velocityChange) * hitVelocityToHealthRatio);
            if (health <= 0.0) {
                cleanUp();
            }
            Entities.editEntity(puppetEntities.healthbar, {
                dimensions: getHealthBarDimensions(),
                // XXX why...?
                localPosition: { x: 0, y: 0, z: 0 },
                localRotation: { x: 0, y: 0, z: 0, w: 1 },
                localAngularVelocity: { x: 0, y: 0, z: 0 },
                localVelocity: { x: 0, y: 0, z: 0 }
            });

            addEntityAuto({
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


    function cleanUp() {
        if (messagesEnabled) {
            Messages.unsubscribe("Puppet-Sword-Fight");
            Messages.messageReceived.disconnect(handleMessages);
        }
        messagesEnabled = false;

        for (var key in puppetEntities) {
            if (puppetEntities.hasOwnProperty(key)) {
                var entityID = puppetEntities[key];
                Entities.deleteEntity(entityID);
            }
        }

        puppetEntities = {};
    }


    this.preload = function (entityID) {
        this.entityID = entityID;
    };

    this.unload = function() {
        cleanUp();
    };

    this.startEquip = function (id, params) {
        health = 100.0;
        this.hand = params[0] == "left" ? 0 : 1;
        this.equipperID = params[1];
        rezSword(this.hand, this.entityID);
        rezShield(this.hand == LEFT_HAND ? RIGHT_HAND : LEFT_HAND, this.entityID, function (shieldID) {
            rezHealthBar(this.hand, shieldID);
        });
        rezTargets(this.entityID);

        Messages.messageReceived.connect(handleMessages);
        Messages.subscribe("Puppet-Sword-Fight");
        messagesEnabled = true;
    };

    this.releaseEquip = function (id, params) {
        this.hand = params[0] == "left" ? 0 : 1;
        this.equipperID = params[1];
        cleanUp();
    };

    Script.scriptEnding.connect(function () {
        cleanUp();
    });

});
