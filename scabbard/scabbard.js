"use strict";

/* global Script, Entities, MyAvatar, Messages, Controller, Settings, Uuid, Vec3, getControllerWorldLocation */


Script.include("/~/system/libraries/controllers.js");

(function() {

    var SCABBARD_SETTINGS = "io.highfidelity.scabbard";
    var DOWN = { x: 0, y: -1, z: 0 };
    var LEFT_HAND = 0;
    var RIGHT_HAND = 1;

    var TRIGGER_OFF_VALUE = 0.1;
    var TRIGGER_ON_VALUE = TRIGGER_OFF_VALUE + 0.05; // Squeezed just enough to activate search or near grab


    function cleanProperties(props) {
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
        delete props.velocity;
        delete props.angularVelocity;
        // delete props.dimensions;
        delete props.renderInfo;
        delete props.lifetime;
        delete props.actionData;
        delete props.position;
        delete props.rotation;
        return props;
    }


    function isNullID(testID) {
        if (!testID) {
            return true;
        } else if (testID == Uuid.NULL) {
            return true;
        } else {
            return false;
        }
    }


    function sortPropertiesByParentage(props) {
        var parentIDs = {};
        for (var i = 0; i < props.length; i++) {
            parentIDs[props.id] = props.parentID;
        }

        function parentCompare(propsA, propsB) {
            // search for a parenting chain from A to B
            var parentID;
            for (parentID = propsA.parentID;
                 !isNullID(parentID);
                 parentID = parentIDs[parentID]) {
                if (parentID == propsB.id) {
                    return 1;
                }
            }

            // search for a parenting chain from B to A
            for (parentID = propsB.parentID;
                 !isNullID(parentID);
                 parentID = parentIDs[parentID]) {
                if (parentID == propsA.id) {
                    return -1;
                }
            }

            // push non-children to the front or the list
            if (!isNullID(propsA.parentID) && isNullID(propsB.parentID)) {
                return 1;
            }
            if (isNullID(propsA.parentID) && !isNullID(propsB.parentID)) {
                return -1;
            }

            // otherwise just sort by id
            return ((propsA.id == propsB.id) ? 0 : ((propsA.id > propsB.id) ? 1 : -1));
        }

        props.sort(parentCompare);
        return props;
    }


    function entitiesIDsToProperties(entityIDs, basePosition) {
        var props = [];
        var actions = [];

        print("entitiesIDsToProperties -- " + JSON.stringify(entityIDs));

        for (var i = 0; i < entityIDs.length; i++) {
            var entityID = entityIDs[i];
            var entityProps = Entities.getEntityProperties(entityID);

            if (!entityProps || !entityProps.localPosition) {
                continue;
            }

            var actionIDs = Entities.getActionIDs(entityID);
            for (var actionIndex = 0; actionIndex < actionIDs.length; actionIndex++) {
                var actionID = actionIDs[actionIndex];
                var actionArgs = Entities.getActionArguments(entityID, actionID);
                if (actionArgs.type == "hold" || actionArgs.type == "fargrab") {
                    continue;
                }

                actionArgs.id = actionID;
                actionArgs.entityID = entityID;
                delete actionArgs["::active"];
                delete actionArgs["::motion-type"];
                delete actionArgs.isMine;
                actions.push(actionArgs);
            }

            entityProps.id = entityID;
            cleanProperties(entityProps);
            props.push(entityProps);
        }

        if (props.length === 0) {
            return null;
        }

        print("--- before sort");
        for (var m = 0; m < props.length; m++) {
            var mProps = props[m];
            print(mProps.id);
        }

        props = sortPropertiesByParentage(props);

        print("--- after sort");

        for (var j = 0; j < props.length; j++) {
            var jProps = props[j];
            print(jProps.id);
            if (isNullID(jProps.parentID)) {
                // for top-level (non-children) entities, delete a few more properties
                delete jProps.parentID;
                delete jProps.parentJointIndex;
                delete jProps.localVelocity;
                delete jProps.localAngularVelocity;
                jProps.localPosition = Vec3.subtract(jProps.localPosition, basePosition);
                // TODO: set localRotation based on a baseRotation parameter
            }
        }

        return {
            Version: 89,
            Entities: props,
            Actions: actions
        };
    }


    function propertiesToEntities(jsonDecoded, basePosition) {
        var props;
        var actions;

        if (jsonDecoded.Entities) {
            props = jsonDecoded.Entities;
            actions = jsonDecoded.Actions;
        } else {
            // assume it's just the properties of one entity
            props = [jsonDecoded];
            actions = [];
        }
        var entityIDMap = {};
        var clientOnly = !(Entities.canRez() || Entities.canRezTmp());

        for (var j = 0; j < props.length; j++) {
            var entityProps = props[j];
            if (isNullID(entityProps.parentID)) {
                entityProps.localPosition = Vec3.sum(basePosition, entityProps.localPosition);
                // TODO: set entityProps.localRotation from a baseRotation parameter
            }

            if (entityProps.parentID && entityIDMap.hasOwnProperty(entityProps.parentID)) {
                entityProps.parentID = entityIDMap[entityProps.parentID];
                // TODO: polyvox neighbors
            }

            var originalID = entityProps.id;
            delete entityProps.id;
            delete entityProps.locked;

            var entityID = Entities.addEntity(entityProps, clientOnly);
            entityIDMap[originalID] = entityID;
        }

        for (var k = 0; k < actions.length; k++) {
            var action = actions[k];

            if (!entityIDMap.hasOwnProperty(action.entityID)) {
                print("Warning: propertiesToEntities -- action on unknown entity: " + action.entityID);
                continue;
            }
            action.entityID = entityIDMap[action.entityID];

            if (action.hasOwnProperty("otherEntityID")) {
                if (!entityIDMap.hasOwnProperty(action.otherEntityID)) {
                    print("Warning: propertiesToEntities -- action on unknown otherEntityID: " + action.otherEntityID);
                    continue;
                }
                action.otherEntityID = entityIDMap[action.otherEntityID];
            }

            var actionEntityID = action.entityID;
            var actionType = action.type;
            delete action.id;
            delete action.type;
            delete action.entityID;
            delete action.ttl;
            // print("QQQQ adding action -- type:" + actionType + " actionEntityID:" + actionEntityID + " action:" +
            //       JSON.stringify(action));
            Entities.addAction(actionType, actionEntityID, action);
        }
    }


    // function getRootIDOfParentingTree(origID) {
    //     while (true) {
    //         var entProps = Entities.getEntityProperties(origID);
    //         if (entProps && entProps.parentID && !isNullID(entProps.parentID)) {
    //             origID = entProps.parentID;
    //         } else {
    //             break;
    //         }
    //     }
    //     return origID;
    // }


    function getConnectedEntityIDs(origID) {
        // recursively get IDs of parents/descendants and of entities connected via dynamics (bullet constraints)
        var toCheck = {};
        toCheck[origID] = false;

        var done = false;
        while (!done) {
            done = true;

            // XXX
            print("----------------");
            for (var entityIDX in toCheck) {
                if (toCheck.hasOwnProperty(entityIDX)) {
                    print(entityIDX + " : " + toCheck[entityIDX]);
                }
            }
            // XXX

            for (var entityID in toCheck) {
                if (toCheck.hasOwnProperty(entityID)) {
                    if (!toCheck[entityID]) {
                        toCheck[entityID] = true;

                        // look for a parent
                        var parentIDProps = Entities.getEntityProperties(entityID, ["parentID"]);
                        if (parentIDProps && !isNullID(parentIDProps.parentID)) {
                            if (!toCheck.hasOwnProperty(parentIDProps.parentID)) {
                                print("found parent: " + parentIDProps.parentID);
                                toCheck[parentIDProps.parentID] = false;
                                done = false;
                            }
                        }

                        // look for children
                        var children = Entities.getChildrenIDs(entityID);
                        for (var c = 0; c < children.length; c++) {
                            var childID = children[c];
                            if (!toCheck.hasOwnProperty(childID)) {
                                print("found child: " + childID);
                                toCheck[childID] = false;
                                done = false;
                            }
                        }

                        // look for actions (bullet contraints) that link to other entities
                        var actionIDs = Entities.getActionIDs(entityID);
                        for (var actionIndex = 0; actionIndex < actionIDs.length; actionIndex++) {
                            var actionID = actionIDs[actionIndex];
                            var actionArgs = Entities.getActionArguments(entityID, actionID);
                            if (actionArgs.hasOwnProperty("otherEntityID")) {
                                if (!toCheck.hasOwnProperty(actionArgs.otherEntityID)) {
                                    print("found contraint link: " + actionArgs.otherEntityID);
                                    toCheck[actionArgs.otherEntityID] = false;
                                    done = false;
                                }
                            }
                        }
                    }
                }
            }
        }

        var result = [];
        for (var f in toCheck) {
            if (toCheck.hasOwnProperty(f)) {
                result.push(f);
            }
        }
        return result;
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
                propertiesToEntities(this.entityInScabbardProps, controllerLocation.position);
                this.entityInScabbardProps = null;

                // var clientOnly = !(Entities.canRez() || Entities.canRezTmp());
                // this.entityInScabbardProps.position = controllerLocation.position;
                // var entityID = Entities.addEntity(this.entityInScabbardProps, clientOnly);
                // this.entityInScabbardProps = null;
            }
        };


        this.handleTriggerValue = function (value) {
            if (this.previousTriggerValue < TRIGGER_ON_VALUE && value >= TRIGGER_ON_VALUE) {
                this.activate();
            }
            this.previousTriggerValue = value;
        };


        this.saveEntityInScabbard = function (targetEntityID, controllerLocation) {
            var entityIDs = getConnectedEntityIDs(targetEntityID);
            var props = entitiesIDsToProperties(entityIDs, controllerLocation.position);
            this.entityInScabbardProps = props;
            Settings.setValue(SCABBARD_SETTINGS + "." + this.hand, JSON.stringify(props));
            for (var i = 0; i < entityIDs.length; i++) {
                Entities.deleteEntity(entityIDs[i]);
            }
        };


        this.checkRelease = function (droppedEntityID) {
            var controllerName = (this.hand === LEFT_HAND) ? Controller.Standard.LeftHand : Controller.Standard.RightHand;
            var controllerLocation = getControllerWorldLocation(controllerName, true);
            if (detectScabbardGesture(controllerLocation, this.hand)) {
                this.saveEntityInScabbard(droppedEntityID, controllerLocation);
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
