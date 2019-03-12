"use strict";

/* global Script, print, Entities, MyAvatar, Messages, Controller, Settings, Vec3, getControllerWorldLocation */


Script.include("/~/system/libraries/controllers.js");

(function() {

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var cleanProperties = EUs.cleanProperties;
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntitiesAuto = EUs.propertiesToEntitiesAuto;
    var getConnectedEntityIDs = EUs.getConnectedEntityIDs;
    var propertySetsAreSimilar = EUs.propertySetsAreSimilar;


    var SCABBARD_SETTINGS = "io.highfidelity.scabbard";
    var DOWN = { x: 0, y: -1, z: 0 };
    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab


    // function detectScabbardGesture(controllerLocation, hand) {
    //     if (! controllerLocation.valid) {
    //         return false;
    //     }
    //
    //     var neckJointIndex = MyAvatar.getJointIndex("Neck");
    //     var avatarFrameNeckPos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(neckJointIndex);
    //     var eyeJointIndex = MyAvatar.getJointIndex("LeftEye");
    //     var avatarFrameEyePos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(eyeJointIndex);
    //
    //     var avatarFrameControllerPos = MyAvatar.worldToJointPoint(controllerLocation.position, -1);
    //     var avatarFrameControllerRot = MyAvatar.worldToJointRotation(controllerLocation.orientation, -1);
    //
    //     if (avatarFrameControllerPos.y > avatarFrameNeckPos.y && // above the neck and
    //         avatarFrameControllerPos.z + 0.08 > avatarFrameEyePos.z) { // (nearly) behind the eyes
    //         var localHandUpAxis = hand === RIGHT_HAND ? { x: 1, y: 0, z: 0 } : { x: -1, y: 0, z: 0 };
    //         var localHandUp = Vec3.multiplyQbyV(avatarFrameControllerRot, localHandUpAxis);
    //         if (Vec3.dot(localHandUp, DOWN) > 0.0) {
    //             return true; // hand is upside-down vs avatar
    //         }
    //     }
    //     return false;
    // }


    function detectScabbardGesture(controllerLocation, hand) {
        if (! controllerLocation.valid) {
            return false;
        }

        var eyeJointIndex = MyAvatar.getJointIndex("LeftEye");
        var avatarFrameEyePos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(eyeJointIndex);
        var shoulderIndex = MyAvatar.getJointIndex(hand === RIGHT_HAND ? "RightShoulder" : "LeftShoulder");
        var shoulderPos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(shoulderIndex);
        var avatarFrameScabbardPoint = { x: shoulderPos.x, y: avatarFrameEyePos.y, z: avatarFrameEyePos.z };

        var avatarFrameControllerPos = MyAvatar.worldToJointPoint(controllerLocation.position, -1);
        // var avatarFrameControllerRot = MyAvatar.worldToJointRotation(controllerLocation.orientation, -1);

        if (Vec3.length(Vec3.subtract(avatarFrameControllerPos, avatarFrameScabbardPoint)) < 0.20) {
            var localHandUpAxis = hand === RIGHT_HAND ? { x: 1, y: 0, z: 0 } : { x: -1, y: 0, z: 0 };
            // var localHandUp = Vec3.multiplyQbyV(avatarFrameControllerRot, localHandUpAxis);
            // if (Vec3.dot(localHandUp, DOWN) > 0.0) {
            //     return true; // hand is upside-down vs avatar
            // }
            return true;
        }
        return false;
    }


    function Scabbard(hand) {
        this.hand = hand;
        this.entityInScabbardProps = null;
        this.previousTriggerValue = 0;
        this.inHandID = null;

        try {
            this.entityInScabbardProps = JSON.parse(Settings.getValue(SCABBARD_SETTINGS + "." + this.hand));
            cleanProperties(this.entityInScabbardProps);
        } catch (err) {
            // don't spam the logs
        }


        this.takeEntityFromScabbard = function () {
            if (!this.entityInScabbardProps) {
                return;
            }

            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            if (detectScabbardGesture(controllerLocation, this.hand)) {
                propertiesToEntitiesAuto(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);

                // this line would make the scabbard empty after an item is taken out:
                // this.entityInScabbardProps = null;
            }
        };


        this.handleTriggerValue = function (value) {
            if (this.inHandID) {
                return;
            }

            if (this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
                this.takeEntityFromScabbard();
            }
            this.previousTriggerValue = value;
        };


        this.saveEntityInScabbard = function (targetEntityID, controllerLocation) {
            var entityIDs = getConnectedEntityIDs(targetEntityID);
            var props = entitiesIDsToProperties(entityIDs, controllerLocation.position, controllerLocation.rotation);
            if (!props) {
                print("WARNING: scabbard.js -- got null properties for IDs: " + JSON.stringify(entityIDs));
                return;
            }

            if (this.entityInScabbardProps && !propertySetsAreSimilar(this.entityInScabbardProps, props)) {
                // the scabbard already had something in it.  if they don't mostly match, kick the old thing
                // out into the world.
                propertiesToEntitiesAuto(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);
            }

            this.entityInScabbardProps = props;
            Settings.setValue(SCABBARD_SETTINGS + "." + this.hand, JSON.stringify(props));
            for (var i = 0; i < entityIDs.length; i++) {
                Entities.deleteEntity(entityIDs[i]);
            }
        };


        this.checkRelease = function (droppedEntityID) {
            this.inHandID = null;
            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);
            if (detectScabbardGesture(controllerLocation, this.hand)) {
                this.saveEntityInScabbard(droppedEntityID, controllerLocation);
            }
        };


        this.noteGrab = function (grabbedEntityID) {
            this.inHandID = grabbedEntityID;
        };


        this.cleanup = function () {
            if (this.debugEntity) {
                Entities.deleteEntity(this.debugEntity);
            }
        }


        this.debug = function () {
            var eyeJointIndex = MyAvatar.getJointIndex("LeftEye");
            var avatarFrameEyePos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(eyeJointIndex);
            var shoulderIndex = MyAvatar.getJointIndex(hand === RIGHT_HAND ? "RightArm" : "LeftArm");
            var shoulderPos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(shoulderIndex);
            var avatarFrameScabbardPoint = { x: shoulderPos.x, y: avatarFrameEyePos.y, z: avatarFrameEyePos.z };

            this.debugEntity = Entities.addEntity({
                name: "scabbard debug entity",
                type: "Sphere",
                color: { red: 220, green: 140, blue: 0 },
                dimensions: 0.20 * 2.0,
                localPosition: avatarFrameScabbardPoint,
                parentID: MyAvatar.sessionUUID,
                parentJointIndex: -1,
                dynamic: false,
                collisionless: true,
                lifetime: 500,
                alpha: 0.8,
                grab: { grabbable: false }
            });
        };
    }


    var leftScabbard = new Scabbard(LEFT_HAND);
    var rightScabbard = new Scabbard(RIGHT_HAND);

    // leftScabbard.debug();
    // rightScabbard.debug();

    function leftTrigger(value) {
        leftScabbard.handleTriggerValue(value);
    }


    function rightTrigger(value) {
        rightScabbard.handleTriggerValue(value);
    }


    function handleMessages(channel, message, sender) {
        var data;
        if (sender === MyAvatar.sessionUUID) {
            if (channel === "Hifi-Object-Manipulation") {
                try {
                    data = JSON.parse(message);
                    if (data.action == "release") {
                        if (data.joint == "RightHand") {
                            rightScabbard.checkRelease(data.grabbedEntity);
                        } else {
                            leftScabbard.checkRelease(data.grabbedEntity);
                        }
                    } else if (data.action == "grab" || data.action == "equip") {
                        if (data.joint == "RightHand") {
                            rightScabbard.noteGrab(data.grabbedEntity);
                        } else {
                            leftScabbard.noteGrab(data.grabbedEntity);
                        }
                    }
                } catch (err) {
                    print("WARNING: scabbard.js -- error reacting to Hifi-Object-Manipulation message: " + message);
                    print(err.message);
                }
            }
        }
    }


    Messages.subscribe("Hifi-Object-Manipulation");
    Messages.messageReceived.connect(handleMessages);

    var mappingName = "Scabbard-Mapping-" + "-" + Math.random();
    var triggerMapping = Controller.newMapping(mappingName);

    triggerMapping.from(Controller.Standard.LT).peek().to(leftTrigger);
    triggerMapping.from(Controller.Standard.RT).peek().to(rightTrigger);

    Controller.enableMapping(mappingName);

    function cleanup() {
        Messages.unsubscribe("Hifi-Object-Manipulation");
        Messages.messageReceived.disconnect(handleMessages);

        triggerMapping.disable();

        leftScabbard.cleanup();
        rightScabbard.cleanup();
    }
    Script.scriptEnding.connect(cleanup);

}());
