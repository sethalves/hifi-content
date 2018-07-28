"use strict";

/* global Script, Entities, MyAvatar, Messages, Controller, Settings, Vec3, getControllerWorldLocation */


Script.include("/~/system/libraries/controllers.js");

(function() {

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var cleanProperties = EUs.cleanProperties;
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntities = EUs.propertiesToEntities;
    var getConnectedEntityIDs = EUs.getConnectedEntityIDs;
    var propertySetsAreSimilar = EUs.propertySetsAreSimilar;


    var SCABBARD_SETTINGS = "io.highfidelity.scabbard";
    var DOWN = { x: 0, y: -1, z: 0 };
    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab


    function detectScabbardGesture(controllerLocation, hand) {
        if (! controllerLocation.valid) {
            return false;
        }

        var neckJointIndex = MyAvatar.getJointIndex("Neck");
        var avatarFrameNeckPos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(neckJointIndex);
        var eyeJointIndex = MyAvatar.getJointIndex("LeftEye");
        var avatarFrameEyePos = MyAvatar.getAbsoluteJointTranslationInObjectFrame(eyeJointIndex);

        var avatarFrameControllerPos = MyAvatar.worldToJointPoint(controllerLocation.position, -1);
        var avatarFrameControllerRot = MyAvatar.worldToJointRotation(controllerLocation.orientation, -1);

        if (avatarFrameControllerPos.y > avatarFrameNeckPos.y && // above the neck and
            avatarFrameControllerPos.z > avatarFrameEyePos.z) { // behind the eyes
            var localHandUpAxis = hand === RIGHT_HAND ? { x: 1, y: 0, z: 0 } : { x: -1, y: 0, z: 0 };
            var localHandUp = Vec3.multiplyQbyV(avatarFrameControllerRot, localHandUpAxis);
            if (Vec3.dot(localHandUp, DOWN) > 0.0) {
                return true; // hand is upside-down vs avatar
            }
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
                propertiesToEntities(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);
                // this line would make the scabbard empty after an item is take out:
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

            if (this.entityInScabbardProps && !propertySetsAreSimilar(this.entityInScabbardProps, props)) {
                // the scabbard already had something in it.  if they don't mostly match, kick the old thing
                // out into the world.
                propertiesToEntities(this.entityInScabbardProps, controllerLocation.position, controllerLocation.rotation);
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
    }


    var leftScabbard = new Scabbard(LEFT_HAND);
    var rightScabbard = new Scabbard(RIGHT_HAND);


    function leftTrigger(value) {
        leftScabbard.handleTriggerValue(value);
    }


    function rightTrigger(value) {
        rightScabbard.handleTriggerValue(value);
    }


    function handleMessage(channel, message, sender) {
        var data;
        if (sender === MyAvatar.sessionUUID) {
            if (channel === 'Hifi-Object-Manipulation') {
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


    function cleanup() {
        menuMapping.disable();
    }

    Messages.subscribe('Hifi-Object-Manipulation');
    Messages.messageReceived.connect(handleMessage);

    var mappingName = 'Scabbard-Mapping-' + "-" + Math.random();
    var menuMapping = Controller.newMapping(mappingName);

    menuMapping.from(Controller.Standard.LT).peek().to(leftTrigger);
    menuMapping.from(Controller.Standard.RT).peek().to(rightTrigger);

    Controller.enableMapping(mappingName);

    Script.scriptEnding.connect(cleanup);

}());
