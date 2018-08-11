"use strict";

/* global module, Script, print, Entities, Uuid, Mat4, Quat */

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
    // delete props.velocity;
    // delete props.angularVelocity;
    delete props.renderInfo;
    delete props.lifetime;
    delete props.actionData;
    delete props.position;
    delete props.rotation;
    delete props.dimensions;
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

    function parentCompare(sortPropsA, sortPropsB) {
        // search for a parenting chain from A to B
        var parentID;
        for (parentID = sortPropsA.parentID;
             !isNullID(parentID);
             parentID = parentIDs[parentID]) {
            if (parentID == sortPropsB.id) {
                return 1;
            }
        }

        // search for a parenting chain from B to A
        for (parentID = sortPropsB.parentID;
             !isNullID(parentID);
             parentID = parentIDs[parentID]) {
            if (parentID == sortPropsA.id) {
                return -1;
            }
        }

        // push non-children to the front or the list
        if (!isNullID(sortPropsA.parentID) && isNullID(sortPropsB.parentID)) {
            return 1;
        }
        if (isNullID(sortPropsA.parentID) && !isNullID(sortPropsB.parentID)) {
            return -1;
        }

        // otherwise just sort by id
        return ((sortPropsA.id == sortPropsB.id) ? 0 : ((sortPropsA.id > sortPropsB.id) ? 1 : -1));
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

    for (var k = 0; k < actions.length; k++) {
        var action = actions[ k ];
        if (action.type == "offset") {
            action.pointToOffsetFrom = Mat4.transformPoint(baseMatInv, action.pointToOffsetFrom);
        }
    }

    return {
        Version: 89,
        Entities: props,
        Actions: actions
    };
}


function checkRezSuccess(entityIDs) {
    for (var i = 0; i < entityIDs.length; i++) {
        var entityID = entityIDs[i];
        var posCheck = Entities.getEntityProperties(entityID, ["localPosition"]).localPosition;
        if (!posCheck || !posCheck.x) {
            return false;
        }
    }
    return true;
}


function propertiesToEntities(jsonDecoded, basePosition, baseRotation, makeAvatarEntities) {
    var baseMat = Mat4.createFromRotAndTrans(baseRotation, basePosition);
    var baseMatRot = Mat4.extractRotation(baseMat);
    var props;
    var actions = [];
    var patchUps = [];

    // deep copy
    props = JSON.parse(JSON.stringify(jsonDecoded.Entities));
    try {
        actions = JSON.parse(JSON.stringify(jsonDecoded.Actions));
    } catch (err) {
        // may not be an Actions section
    }

    var newEntityIDs = [];
    var entityIDMap = {};

    var propsToAdjustWithMap = ["parentID",
                                "xNNeighborID", "yNNeighborID", "zNNeighborID",
                                "xPNeighborID", "yPNeighborID", "zPNeighborID"];

    var failed = [];
    for (var j = 0; j < props.length; j++) {
        var entityProps = props[j];

        if (!entityProps.hasOwnProperty("localPosition")) {
            entityProps.localPosition = entityProps.position;
            delete entityProps.position;
        }
        if (!entityProps.hasOwnProperty("localRotation")) {
            entityProps.localRotation = entityProps.rotation;
            delete entityProps.rotation;
        }
        if (!entityProps.hasOwnProperty("localDimensions")) {
            entityProps.localDimensions = entityProps.dimensions;
            delete entityProps.dimensions;
        }

        if (isNullID(entityProps.parentID)) {
            entityProps.localPosition = Mat4.transformPoint(baseMat, entityProps.localPosition);
            entityProps.localRotation = Quat.multiply(baseMatRot, entityProps.localRotation);
        }

        var originalID = entityProps.id;
        delete entityProps.id;

        for (var t = 0; t < propsToAdjustWithMap.length; t++) {
            var propName = propsToAdjustWithMap[t];
            if (entityProps[propName] && !isNullID(entityProps[propName])) {
                if (entityIDMap.hasOwnProperty(entityProps[propName])) {
                    entityProps[propName] = entityIDMap[entityProps[propName]];
                } else {
                    var patch = { id: originalID };
                    patch[propName] = entityProps[propName];
                    if (propName == "parentID") {
                        print("Warning: propertiesToEntities -- parent sorting failed: " +
                              originalID + " --> " + entityProps[propName]);
                        // if we patch just the parentID, the positions will end up wrong.  include local offsets:
                        patch.localPosition = entityProps.localPosition;
                        patch.localRotation = entityProps.localRotation;
                    }
                    patchUps.push(patch);
                }
            }
        }

        delete entityProps.locked;

        var entityID = Entities.addEntity(entityProps, makeAvatarEntities);
        if (isNullID(entityID)) {
            print("Warning: propertiesToEntities -- addEntity failed: " + JSON.stringify(entityProps));
            entityProps.id = originalID;
            failed.push(entityProps);
        } else {
            newEntityIDs.push(entityID);
            entityIDMap[originalID] = entityID;
        }
    }

    for (var f = 0; f < failed.length; f++) {
        var retryProps = failed[f];
        var retryOriginalID = retryProps.id;
        delete retryProps.id;

        var retryID = Entities.addEntity(retryProps, makeAvatarEntities);
        if (isNullID(retryID)) {
            print("Warning: propertiesToEntities -- addEntity failed again: " + JSON.stringify(retryProps));
        } else {
            newEntityIDs.push(retryID);
            entityIDMap[retryOriginalID] = retryID;
        }
    }

    // in some cases, the order that the entities were rezzed in keeps us from correctly setting
    // properties that refer to other entities
    for (var p = 0; p < patchUps.length; p++) {
        var patchUp = patchUps[p];
        var patchOrigID = patchUp.id;
        delete patchUp.id;

        if (entityIDMap[patchOrigID]) {
            var patchUpEntityID = entityIDMap[patchOrigID];

            for (var patchKey in patchUp) {
                if (patchUp.hasOwnProperty(patchKey) && propsToAdjustWithMap.indexOf(patchKey) >= 0) {
                    var oldIDValue = patchUp[patchKey];
                    if (entityIDMap[oldIDValue]) {
                        patchUp[patchKey] = entityIDMap[oldIDValue];
                    } else {
                        print("Warning: propertiesToEntities -- map doesn't contain entity: " + oldIDValue);
                    }
                }
            }

            Entities.editEntity(patchUpEntityID, patchUp);
        } else {
            print("Warning: propertiesToEntities -- map doesn't contain this entity: " + patchOrigID + " -- " +
                  JSON.stringify(patchUp));
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

        if (action.type == "offset") {
            action.pointToOffsetFrom = Mat4.transformPoint(baseMat, action.pointToOffsetFrom);
        }

       Entities.addAction(actionType, actionEntityID, action);
    }

    return newEntityIDs;
}


function propertiesToEntitiesAuto(jsonDecoded, basePosition, baseRotation) {
    // attempt to rez domain-entities (perhaps tmp), and if that fails, use avatar-entities
    var makeAvatarEntities = !(Entities.canRez() || Entities.canRezTmp());
    var newEntityIDs = propertiesToEntities(jsonDecoded, basePosition, baseRotation, makeAvatarEntities);
    if (!makeAvatarEntities) {
        // some domains allow rezzing but then block it with server-side filters.
        // wait 1/5 of a second and make sure the entities made it.
        Script.setTimeout(function() {
            var success = checkRezSuccess(newEntityIDs);
            if (!success) {
                for (var i = 0; i < newEntityIDs.length; i++) {
                    Entities.deleteEntity(newEntityIDs[i]);
                }
                propertiesToEntities(jsonDecoded, basePosition, baseRotation, true);
            }
        }, 200);
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

                    var props = Entities.getEntityProperties(entityID, ["parentID", "position"]);
                    if (!props) {
                        continue;
                    }

                    // look for a parent
                    if (!isNullID(props.parentID)) {
                        if (!toCheck.hasOwnProperty(props.parentID)) {
                            toCheck[props.parentID] = false;
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

                    // look for actions that link from other entities
                    var REVERSE_CONSTRAINT_SEARCH_RADIUS = 3.0;
                    var nearByEntityIDs = Entities.findEntities(props.position, REVERSE_CONSTRAINT_SEARCH_RADIUS);
                    for (var i = 0; i < nearByEntityIDs.length; i++) {
                        var nearByEntityID = nearByEntityIDs[i];
                        var nearbyActionIDs = Entities.getActionIDs(nearByEntityID);
                        for (var nbActionIndex = 0; nbActionIndex < nearbyActionIDs.length; nbActionIndex++) {
                            var nbActionID = nearbyActionIDs[nbActionIndex];
                            var nbActionArgs = Entities.getActionArguments(nearByEntityID, nbActionID);
                            if (nbActionArgs.hasOwnProperty("otherEntityID") &&
                                nbActionArgs.otherEntityID == entityID) {
                                if (!toCheck.hasOwnProperty(nearByEntityID)) {
                                    toCheck[nearByEntityID] = false;
                                    done = false;
                                }
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


// https://stackoverflow.com/questions/201183/how-to-determine-equality-for-two-javascript-objects#16788517
function objectsAlmostEqual(x, y) {
    // test for deep equality, but allow some slack in numbers
    if (x === null || x === undefined || y === null || y === undefined) { return x === y; }
    // after this just checking type of one would be enough
    if (x.constructor !== y.constructor) { return false; }
    // if they are functions, they should exactly refer to same one (because of closures)
    if (x instanceof Function) { return x === y; }
    // if they are regexps, they should exactly refer to same one (it is hard to better equality check on current ES)
    if (x instanceof RegExp) { return x === y; }

    if (typeof x == typeof 5 && typeof y == typeof 5) {
        return Math.abs(x - y) < 0.003;
    }

    if (x === y || x.valueOf() === y.valueOf()) { return true; }
    if (Array.isArray(x) && x.length !== y.length) { return false; }

    // if they are dates, they must had equal valueOf
    if (x instanceof Date) { return false; }

    // if they are strictly equal, they both need to be object at least
    if (!(x instanceof Object)) { return false; }
    if (!(y instanceof Object)) { return false; }

    // recursive object equality check
    var p = Object.keys(x);
    return Object.keys(y).every(function (i) { return p.indexOf(i) !== -1; }) &&
        p.every(function (i) { return objectsAlmostEqual(x[i], y[i]); });
}


function propertySetsAreSimilar(propsA, propsB) {
    // propsA and propsB should both be the sort of thing returned by entitiesIDsToProperties:
    // { Entities: [ ... ], Actions: [ ... ] }
    // TODO -- examine the parent/child and neighbor relationships and the actions

    if (propsA.Entities.length != propsB.Entities.length) {
        // print("QQQQ propsA.Entities.length != propsB.Entities.length -- " +
        //       propsA.Entities.length + " " + propsB.Entities.length);
        return false;
    }

    var entityPropsACopy = JSON.parse(JSON.stringify(propsA.Entities));
    var entityPropsBCopy = JSON.parse(JSON.stringify(propsB.Entities));

    var scrubPropsList = function(lst) {
        var propsToDelete = ["id", "parentID", "localVelocity", "localAngularVelocity", "actionData",
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
                delete lst[j].parentJointIndex;
                delete lst[j].localVelocity;
                delete lst[j].localAngularVelocity;
            }

            if (lst[j].type == "Line") {
                // line dimensions seem to change a lot
                delete lst[j].localDimensions;
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

    // var result = (JSON.stringify(entityPropsACopy) == JSON.stringify(entityPropsBCopy));
    // they are sorted by IDs, and these have changed, so the above line can fail.
    for (var k = 0; k < entityPropsACopy.length; k++) {
        var aCopy = entityPropsACopy[k];
        var foundMatch = false;
        for (var m = 0; m < entityPropsBCopy.length; m++) {
            var bCopy = entityPropsBCopy[m];
            // if (JSON.stringify(propA) == JSON.stringify(propB)) {
            if (objectsAlmostEqual(aCopy, bCopy)) {
                foundMatch = true;
                if (m === 0) {
                    entityPropsBCopy.shift();
                }
                break;
            }
        }
        if (!foundMatch) {
            // print("QQQQ no match for " + JSON.stringify(aCopy));
            // print("QQQQ similar result = false");
            return false;
        }
    }

    // print("QQQQ similar result = true");
    return true;
}


module.exports = {
    cleanProperties: cleanProperties,
    isNullID: isNullID,
    sortPropertiesByParentage: sortPropertiesByParentage,
    entitiesIDsToProperties: entitiesIDsToProperties,
    checkRezSuccess: checkRezSuccess,
    propertiesToEntities: propertiesToEntities,
    propertiesToEntitiesAuto: propertiesToEntitiesAuto,
    getRootIDOfParentingTree: getRootIDOfParentingTree,
    getConnectedEntityIDs: getConnectedEntityIDs,
    propertySetsAreSimilar: propertySetsAreSimilar
};
