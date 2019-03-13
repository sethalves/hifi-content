"use strict";

/* global Script, print, Entities, MyAvatar, Messages, Controller, Settings, Vec3, getControllerWorldLocation */


Script.include("/~/system/libraries/controllers.js");

(function() {
    var AppUi = Script.require("appUi");
    var ui;

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var cleanProperties = EUs.cleanProperties;
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntitiesAuto = EUs.propertiesToEntitiesAuto;
    var getConnectedEntityIDs = EUs.getConnectedEntityIDs;
    var propertySetsAreSimilar = EUs.propertySetsAreSimilar;

    var SCABBARD_SETTINGS = "io.highfidelity.scabbard";
    // var DOWN = { x: 0, y: -1, z: 0 };
    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var SHOULDER = 0;
    var HIP = 1;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab

    var mappingName;
    var triggerMapping;


    // function detectShoulderGesture(controllerLocation, hand) {
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


    function getAvatarFrameScabbardPoint(hipOrShoulder, hand) {
        if (hipOrShoulder == SHOULDER) {
            var eyeJointIndex = MyAvatar.getJointIndex("LeftEye");
            var avatarFrameEyePos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(eyeJointIndex);
            var shoulderIndex = MyAvatar.getJointIndex(hand === RIGHT_HAND ? "RightShoulder" : "LeftShoulder");
            var shoulderPos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(shoulderIndex);
            return { x: shoulderPos.x, y: avatarFrameEyePos.y, z: 0.0 };
        } else {
            return {
                x: hand == LEFT_HAND ? -0.15 : 0.15,
                y: 0.07,
                z: 0.08
            };
        }
    }


    function detectShoulderGesture(controllerLocation, hand) {
        if (! controllerLocation.valid) {
            return false;
        }

        var avatarFrameScabbardPoint = getAvatarFrameScabbardPoint(SHOULDER, hand);
        var avatarFrameControllerPos = MyAvatar.worldToJointPoint(controllerLocation.position, -1);
        // var avatarFrameControllerRot = MyAvatar.worldToJointRotation(controllerLocation.orientation, -1);

        if (Vec3.length(Vec3.subtract(avatarFrameControllerPos, avatarFrameScabbardPoint)) < 0.2) {
            // var localHandUpAxis = hand === RIGHT_HAND ? { x: 1, y: 0, z: 0 } : { x: -1, y: 0, z: 0 };
            // var localHandUp = Vec3.multiplyQbyV(avatarFrameControllerRot, localHandUpAxis);
            // if (Vec3.dot(localHandUp, DOWN) > 0.0) {
            //     return true; // hand is upside-down vs avatar
            // }
            return true;
        }
        return false;
    }


    function detectHipGesture(controllerLocation, hand) {
        if (! controllerLocation.valid) {
            return false;
        }

        var avatarFrameScabbardPoint = getAvatarFrameScabbardPoint(HIP, hand);
        var avatarFrameControllerPos = MyAvatar.worldToJointPoint(controllerLocation.position, -1);
        return Vec3.length(Vec3.subtract(avatarFrameControllerPos, avatarFrameScabbardPoint)) < 0.2;
    }


    function Scabbard(hand, hipOrShoulder) {
        this.hand = hand;
        this.hipOrShoulder = hipOrShoulder;
        this.entityInScabbardProps = null;
        this.previousTriggerValue = 0;
        this.inHandID = null;

        this.idStr = (this.hand == LEFT_HAND ? "left" : "right") + (hipOrShoulder == HIP ? "Hip" : "Shoulder");

        this.enabled = Settings.getValue(SCABBARD_SETTINGS + "." + this.idStr + "Enabled", true);
        this.locked = Settings.getValue(SCABBARD_SETTINGS + "." + this.idStr + "Locked", false);

        try {
            this.entityInScabbardProps = JSON.parse(Settings.getValue(SCABBARD_SETTINGS + "." + this.idStr));
            cleanProperties(this.entityInScabbardProps);
        } catch (err) {
            // don't spam the logs
        }

        this.initUI = function () {
            ui.sendMessage({'method' : this.idStr + "Enabled", 'value' : this.enabled});
            ui.sendMessage({'method' : this.idStr + "Locked", 'value' : this.locked});
        };

        this.setEnabled = function (value) {
            this.enabled = value;
            Settings.setValue(SCABBARD_SETTINGS + "." + this.idStr + "Enabled", value);
        };

        this.setLocked = function (value) {
            this.locked = value;
            Settings.setValue(SCABBARD_SETTINGS + "." + this.idStr + "Locked", value);
        };


        this.takeEntityFromScabbard = function () {
            if (!this.entityInScabbardProps) {
                return;
            }

            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            if ((this.hipOrShoulder == SHOULDER && detectShoulderGesture(controllerLocation, this.hand)) ||
                (this.hipOrShoulder == HIP && detectHipGesture(controllerLocation, this.hand))) {
                propertiesToEntitiesAuto(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);

                // this line would make the scabbard empty after an item is taken out:
                // this.entityInScabbardProps = null;
            }
        };


        this.handleTriggerValue = function (value) {
            if (this.inHandID) {
                return;
            }

            if (this.enabled && this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
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

            var areSimilar = this.entityInScabbardProps && propertySetsAreSimilar(this.entityInScabbardProps, props);

            if (!this.locked && this.entityInScabbardProps && !areSimilar) {
                // the scabbard already had something in it.  if they don't mostly match, kick the old thing
                // out into the world.
                propertiesToEntitiesAuto(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);
                areSimilar = true;
            }

            if (!this.locked) {
                this.entityInScabbardProps = props;
                Settings.setValue(SCABBARD_SETTINGS + "." + this.idStr, JSON.stringify(props));
            }

            if (areSimilar) {
                for (var i = 0; i < entityIDs.length; i++) {
                    Entities.deleteEntity(entityIDs[i]);
                }
            }
        };


        this.checkRelease = function (droppedEntityID) {
            this.inHandID = null;
            if (!this.enabled) {
                return;
            }
            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);
            if ((this.hipOrShoulder == SHOULDER && detectShoulderGesture(controllerLocation, this.hand)) ||
                (this.hipOrShoulder == HIP && detectHipGesture(controllerLocation, this.hand))) {
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
        };


        this.debug = function () {
            var avatarFrameScabbardPoint = getAvatarFrameScabbardPoint(this.hipOrShoulder, this.hand);
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

    var leftShoulderScabbard = new Scabbard(LEFT_HAND, SHOULDER);
    var rightShoulderScabbard = new Scabbard(RIGHT_HAND, SHOULDER);
    var leftHipScabbard = new Scabbard(LEFT_HAND, HIP);
    var rightHipScabbard = new Scabbard(RIGHT_HAND, HIP);


    function leftTrigger(value) {
        leftShoulderScabbard.handleTriggerValue(value);
        leftHipScabbard.handleTriggerValue(value);
    }

    function rightTrigger(value) {
        rightShoulderScabbard.handleTriggerValue(value);
        rightHipScabbard.handleTriggerValue(value);
    }


    function handleMessages(channel, message, sender) {
        var data;
        if (sender === MyAvatar.sessionUUID) {
            if (channel === "Hifi-Object-Manipulation") {
                // try {
                    data = JSON.parse(message);
                    if (data.action == "release") {
                        if (data.joint == "RightHand") {
                            rightShoulderScabbard.checkRelease(data.grabbedEntity);
                            rightHipScabbard.checkRelease(data.grabbedEntity);
                        } else {
                            leftShoulderScabbard.checkRelease(data.grabbedEntity);
                            leftHipScabbard.checkRelease(data.grabbedEntity);
                        }
                    } else if (data.action == "grab" || data.action == "equip") {
                        if (data.joint == "RightHand") {
                            rightShoulderScabbard.noteGrab(data.grabbedEntity);
                            rightHipScabbard.noteGrab(data.grabbedEntity);
                        } else {
                            leftShoulderScabbard.noteGrab(data.grabbedEntity);
                            leftHipScabbard.noteGrab(data.grabbedEntity);
                        }
                    }
                // } catch (err) {
                //     print("WARNING: scabbard.js -- error reacting to Hifi-Object-Manipulation message: " + message);
                //     print(err.message);
                // }
            }
        }
    }

    function cleanup() {
        Messages.unsubscribe("Hifi-Object-Manipulation");
        Messages.messageReceived.disconnect(handleMessages);

        triggerMapping.disable();

        leftShoulderScabbard.cleanup();
        rightShoulderScabbard.cleanup();
        leftHipScabbard.cleanup();
        rightHipScabbard.cleanup();
    }


    function initUI() {
        leftShoulderScabbard.initUI();
        rightShoulderScabbard.initUI();
        leftHipScabbard.initUI();
        rightHipScabbard.initUI();
    }


    ui = new AppUi({
        buttonName: "Scabbard",
        home: Script.resolvePath("scabbard.qml"),
        onMessage: fromQml,
        onOpened: initUI
        // normalButton: "icons/tablet-icons/avatar-i.svg",
        // activeButton: "icons/tablet-icons/avatar-a.svg",
    });


    // leftShoulderScabbard.debug();
    // rightShoulderScabbard.debug();
    // leftHipScabbard.debug();
    // rightHipScabbard.debug();

    Messages.subscribe("Hifi-Object-Manipulation");
    Messages.messageReceived.connect(handleMessages);

    mappingName = "Scabbard-Mapping-" + "-" + Math.random();
    triggerMapping = Controller.newMapping(mappingName);

    triggerMapping.from(Controller.Standard.LT).peek().to(leftTrigger);
    triggerMapping.from(Controller.Standard.RT).peek().to(rightTrigger);

    Controller.enableMapping(mappingName);

    Script.scriptEnding.connect(cleanup);


    function fromQml(message) {
        print("message from qml: " + JSON.stringify(message));
        if (message.method == "leftShoulderEnabled") { leftShoulderScabbard.setEnabled(message.value); }
        if (message.method == "leftShoulderLocked") { leftShoulderScabbard.setLocked(message.value); }
        if (message.method == "rightShoulderEnabled") { rightShoulderScabbard.setEnabled(message.value); }
        if (message.method == "rightShoulderLocked") { rightShoulderScabbard.setLocked(message.value); }
        if (message.method == "leftHipEnabled") { leftHipScabbard.setEnabled(message.value); }
        if (message.method == "leftHipLocked") { leftHipScabbard.setLocked(message.value); }
        if (message.method == "rightHipEnabled") { rightHipScabbard.setEnabled(message.value); }
        if (message.method == "rightHipLocked") { rightHipScabbard.setLocked(message.value); }
    }



}());
