"use strict";

/* global Script, Entities, MyAvatar, Messages, Controller, Settings, Vec3, getControllerWorldLocation */


Script.include("/~/system/libraries/controllers.js");

(function() {

    var SCABBARD_SETTINGS = "io.highfidelity.scabbard";
    var DOWN = { x: 0, y: -1, z: 0 };
    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab


    function cleanProperties(props) {
        delete props.id;
        delete props.clientOnly;
        delete props.created;
        delete props.lastEdited;
        delete props.lastEditedBy;
        delete props.owningAvatarID;
        delete props.queryAACube;
        delete props.age;
        delete props.ageAsText;
        delete props.naturalDimensions;
        delete props.naturalPosition;
        delete props.acceleration;
        delete props.scriptTimestamp;
        delete props.boundingBox;
        delete props.position;
        delete props.rotation;
        delete props.velocity;
        delete props.angularVelocity;
        delete props.dimensions;
        delete props.renderInfo;
        delete props.parentID;
        delete props.parentJointIndex;
        delete props.localPosition;
        delete props.localRotation;
        delete props.lifetime;
        delete props.actionData;
        delete props.localVelocity;
        delete props.localAngularVelocity;
        return props;
    }


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

        try {
            this.entityInScabbardProps = JSON.parse(Settings.getValue(SCABBARD_SETTINGS + "." + this.hand));
            cleanProperties(this.entityInScabbardProps);
        } catch (err) {
            // don't spam the logs
        }


        this.activate = function () {
            if (!this.entityInScabbardProps) {
                return;
            }

            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);

            if (detectScabbardGesture(controllerLocation, this.hand)) {
                var clientOnly = !(Entities.canRez() || Entities.canRezTmp());
                this.entityInScabbardProps.position = controllerLocation.position;
                var entityID = Entities.addEntity(this.entityInScabbardProps, clientOnly);
                this.entityInScabbardProps = null;
            }
        };


        this.handleTriggerValue = function (value) {
            if (this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
                this.activate();
            }
            this.previousTriggerValue = value;
        };


        this.saveEntityInScabbard = function (targetEntityID) {
            var props = Entities.getEntityProperties(targetEntityID);
            if (!props || !props.localPosition) {
                return;
            }
            cleanProperties(props);
            this.entityInScabbardProps = props;
            Settings.setValue(SCABBARD_SETTINGS + "." + this.hand, JSON.stringify(props));
            Entities.deleteEntity(targetEntityID);
        };


        this.checkRelease = function (droppedEntityID) {
            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);
            if (detectScabbardGesture(controllerLocation, this.hand)) {
                this.saveEntityInScabbard(droppedEntityID);
            }
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
                    }
                } catch (e) {
                    print("WARNING: scabbard.js -- error parsing Hifi-Object-Manipulation message: " + message);
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
