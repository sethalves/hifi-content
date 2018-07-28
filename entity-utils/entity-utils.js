"use strict";

/* global module, Entities, Uuid, Mat4, Quat */

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


function entitiesIDsToProperties(entityIDs, basePosition, baseRotation) {
    var baseMat = Mat4.createFromRotAndTrans(baseRotation, basePosition);
    var baseMatInv = Mat4.inverse(baseMat);
    var baseMatInvRot = Mat4.extractRotation(baseMatInv);
    var props = [];
    var actions = [];

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

    props = sortPropertiesByParentage(props);

    for (var j = 0; j < props.length; j++) {
        var jProps = props[j];
        if (isNullID(jProps.parentID)) {
            // for top-level (non-children) entities, delete a few more properties
            delete jProps.parentID;
            delete jProps.parentJointIndex;
            delete jProps.localVelocity;
            delete jProps.localAngularVelocity;
            jProps.localPosition = Mat4.transformPoint(baseMatInv, jProps.localPosition);
            jProps.localRotation = Quat.multiply(baseMatInvRot, jProps.localRotation);
        }
    }

    return {
        Version: 89,
        Entities: props,
        Actions: actions
    };
}


function propertiesToEntities(jsonDecoded, basePosition, baseRotation) {
    var baseMat = Mat4.createFromRotAndTrans(baseRotation, basePosition);
    var baseMatRot = Mat4.extractRotation(baseMat);
    var props;
    var actions;
    var patchUps = [];

    // deep copy
    props = JSON.parse(JSON.stringify(jsonDecoded.Entities));
    actions = JSON.parse(JSON.stringify(jsonDecoded.Actions));

    var entityIDMap = {};
    var clientOnly = !(Entities.canRez() || Entities.canRezTmp());

    for (var j = 0; j < props.length; j++) {
        var entityProps = props[j];
        if (isNullID(entityProps.parentID)) {
            entityProps.localPosition = Mat4.transformPoint(baseMat, entityProps.localPosition);
            entityProps.localRotation = Quat.multiply(baseMatRot, entityProps.localRotation);
        }

        var propsToAdjustWithMap = ["parentID",
                                    "xNNeighborID", "yNNeighborID", "zNNeighborID",
                                    "xPNeighborID", "yPNeighborID", "zPNeighborID"];
        for (var t = 0; t < propsToAdjustWithMap.length; t++) {
            var propName = propsToAdjustWithMap[t];
            if (entityProps[propName]) {
                if (entityIDMap.hasOwnProperty(entityProps[propName])) {
                    entityProps[propName] = entityIDMap[entityProps[propName]];
                } else {
                    if (propName == "parentID") {
                        print("Warning: propertiesToEntities -- parent sorting failed: " +
                              entityProps.id + " --> " + entityProps[propName]);
                    }
                    patchUps.push([entityProps.id, propName, entityProps[propName]]);
                }
            }
        }

        var originalID = entityProps.id;
        delete entityProps.id;
        delete entityProps.locked;

        var entityID = Entities.addEntity(entityProps, clientOnly);
        entityIDMap[originalID] = entityID;
    }

    // in some cases, the order that the entities were rezzed in keeps us from correctly setting
    // properties that refer to other entities
    for (var p = 0; p < patchUps.length; p++) {
        var patchUp = patchUps[p];
        var patchUpEntityID = patchUp[0];
        var patchUpPropName = patchUp[1];
        var patchUpPropValue = patchUp[2];
        if (patchUpPropValue && entityIDMap.hasOwnProperty(patchUpPropValue)) {
            var patchUpProps = {};
            patchUpProps[patchUpPropName] = entityIDMap[patchUpPropValue];
            if (entityIDMap[patchUpEntityID]) {
                Entities.editEntity(entityIDMap[patchUpEntityID], patchUpProps);
            } else {
                print("Warning: propertiesToEntities -- map doesn't contain entity: " + patchUpEntityID);
            }
        }
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
        Entities.addAction(actionType, actionEntityID, action);
    }
}


function getRootIDOfParentingTree(origID) {
    while (true) {
        var entProps = Entities.getEntityProperties(origID);
        if (entProps && entProps.parentID && !isNullID(entProps.parentID)) {
            origID = entProps.parentID;
        } else {
            break;
        }
    }
    return origID;
}


function getConnectedEntityIDs(origID) {
    // recursively get IDs of parents/descendants and of entities connected via dynamics (bullet constraints)
    var toCheck = {};
    toCheck[origID] = false;

    var done = false;
    while (!done) {
        done = true;

        for (var entityID in toCheck) {
            if (toCheck.hasOwnProperty(entityID)) {
                if (!toCheck[entityID]) {
                    toCheck[entityID] = true;

                    // look for a parent
                    var parentIDProps = Entities.getEntityProperties(entityID, ["parentID"]);
                    if (parentIDProps && !isNullID(parentIDProps.parentID)) {
                        if (!toCheck.hasOwnProperty(parentIDProps.parentID)) {
                            toCheck[parentIDProps.parentID] = false;
                            done = false;
                        }
                    }

                    // look for children
                    var children = Entities.getChildrenIDs(entityID);
                    for (var c = 0; c < children.length; c++) {
                        var childID = children[c];
                        if (!toCheck.hasOwnProperty(childID)) {
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


function propertySetsAreSimilar(propsA, propsB) {
    // propsA and propsB should both be the sort of thing returned by entitiesIDsToProperties:
    // { Entities: [ ... ], Actions: [ ... ] }
    // TODO -- examine the parent/child and neighbor relationships and the actions


    if (propsA.Entities.length != propsB.Entities.length) {
        // print("QQQQ not similar due to length, " + propsA.Entities.length + " vs " + propsB.Entities.length);

        for (var i = 0; i < propsA.Entities.length || i < propsB.Entities.length; i++) {
            var aName = "None";
            if (i < propsA.Entities.length) {
                aName = propsA.Entities[i].name;
            }
            var bName = "None";
            if (i < propsB.Entities.length) {
                bName = propsB.Entities[i].name;
            }
            // print(i + ": " + aName + " vs " + bName);
        }

        return false;
    }

    var entityPropsACopy = JSON.parse(JSON.stringify(propsA.Entities));
    var entityPropsBCopy = JSON.parse(JSON.stringify(propsB.Entities));

    // print("QQQQ before scrub -- entityPropsACopy = " + JSON.stringify(entityPropsACopy));
    // print("QQQQ before scrub -- entityPropsBCopy = " + JSON.stringify(entityPropsBCopy));

    var scrubPropsList = function(lst) {
        var propsToDelete = ["id", "parentID", "localVelocity", "localAngularVelocity",
                             "xNNeighborID", "yNNeighborID", "zNNeighborID",
                             "xPNeighborID", "yPNeighborID", "zPNeighborID"];

        for (var i = 0; i < lst.length; i++) {
            cleanProperties(lst[i]);
        }

        sortPropertiesByParentage(lst);

        for (var j = 0; j < lst.length; j++) {
            if (isNullID(lst[j].parentID)) {
                delete lst[j].localPosition;
                delete lst[j].localRotation;
            }

            for (var t = 0; t < propsToDelete.length; t++) {
                var propName = propsToDelete[t];
                delete lst[j][propName];
            }
        }
    };

    scrubPropsList(entityPropsACopy);
    scrubPropsList(entityPropsBCopy);

    // print("QQQQ entityPropsACopy = " + JSON.stringify(entityPropsACopy));
    // print("QQQQ entityPropsBCopy = " + JSON.stringify(entityPropsBCopy));

    var result = (JSON.stringify(entityPropsACopy) == JSON.stringify(entityPropsBCopy));

    // print("QQQQ similar result = " + result);
    return result;
}


module.exports = {
    cleanProperties: cleanProperties,
    isNullID: isNullID,
    sortPropertiesByParentage: sortPropertiesByParentage,
    entitiesIDsToProperties: entitiesIDsToProperties,
    propertiesToEntities: propertiesToEntities,
    getRootIDOfParentingTree: getRootIDOfParentingTree,
    getConnectedEntityIDs: getConnectedEntityIDs,
    propertySetsAreSimilar: propertySetsAreSimilar
};
