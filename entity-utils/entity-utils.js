"use strict";

/* global module, Script, print, Entities, Uuid, Mat4, Quat */


function isNullID(testID) {
    if (!testID) {
        return true;
    } else if (testID == Uuid.NULL) {
        return true;
    } else {
        return false;
    }
}


// https://stackoverflow.com/questions/201183/how-to-determine-equality-for-two-javascript-objects#16788517
function objectsAlmostEqual(x, y, tolerance) {
    if (!tolerance) {
        tolerance = 0.003;
    }

    // test for deep equality, but allow some slack in numbers
    if (x === null || x === undefined || y === null || y === undefined) { return x === y; }
    // after this just checking type of one would be enough
    if (x.constructor !== y.constructor) { return false; }
    // if they are functions, they should exactly refer to same one (because of closures)
    if (x instanceof Function) { return x === y; }
    // if they are regexps, they should exactly refer to same one (it is hard to better equality check on current ES)
    if (x instanceof RegExp) { return x === y; }

    if (typeof x == typeof 5 && typeof y == typeof 5) {
        return Math.abs(x - y) < tolerance;
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


var defaultProperties = {
    // "id": "{4fceaa8e-3dbf-4be6-a47d-80d1739340f9}",
    // "type": "Box",
    // "created": "2018-08-11T16:20:28Z",
    // "age": 36.048988342285156,
    // "ageAsText": "0 hours 0 minutes 36 seconds",
    // "lastEdited": 1534004501633529,
    // "lastEditedBy": "{5954f0e8-a28f-44aa-89f3-3dccf721e934}",
    "position": { "x": 0, "y": 0, "z": 0 },
    // "dimensions": { "x": 0.1, "y": 0.1, "z": 0.1 },
    // "naturalDimensions": { "x": 1, "y": 1, "z": 1 },
    // "naturalPosition": { "x": 0, "y": 0, "z": 0 },
    "rotation": { "x": 0, "y": 0, "z": 0, "w": 1 },
    "velocity": { "x": 0, "y": 0, "z": 0 },
    "gravity": { "x": 0, "y": 0, "z": 0 },
    // "acceleration": { "x": 0, "y": 0, "z": 0 },
    "damping": 0.39346998929977417, // actually 0.39347, but ieee
    "restitution": 0.5,
    "friction": 0.5,
    "density": 1000,
    "lifetime": -1,
    "script": "",
    // "scriptTimestamp": 0,
    "serverScripts": "",
    "registrationPoint": { "x": 0.5, "y": 0.5, "z": 0.5 },
    "angularVelocity": { "x": 0, "y": 0, "z": 0 },
    "angularDamping": 0.39346998929977417, // actually 0.39347, but ieee
    "visible": true,
    "canCastShadow": true,
    "collisionless": false,
    // "ignoreForCollisions": false, // ? XXX
    // "collisionMask": 31,
    "collidesWith": "static,dynamic,kinematic,myAvatar,otherAvatar,",
    "dynamic": false,
    "collisionsWillMove": false,
    "href": "",
    "description": "",
    "actionData": "",
    "locked": false,
    "userData": "",
    "alpha": 1,
    "itemName": "",
    "itemDescription": "",
    "itemCategories": "",
    "itemArtist": "",
    "itemLicense": "",
    "limitedRun": 4294967295,
    "marketplaceID": "",
    "editionNumber": 0,
    "entityInstanceNumber": 0,
    "certificateID": "",
    "staticCertificateVersion": 0,
    "name": "",
    "collisionSoundURL": "",
    // "color": { "red": 255, "green": 255, "blue": 255 },
    // "shape": "Cube",
    // "boundingBox": {...},
    "originalTextures": "{\n}\n",
    "parentID": "{00000000-0000-0000-0000-000000000000}",
    "parentJointIndex": 65535,
    // "queryAACube": { "x": 0, "y": 0, "z": 0, "scale": 1 },
    "localPosition": { "x": 0, "y": 0, "z": 0 },
    "localRotation": { "x": 0, "y": 0, "z": 0, "w": 1 },
    "localVelocity": { "x": 0, "y": 0, "z": 0 },
    "localAngularVelocity": { "x": 0, "y": 0, "z": 0 },
    "localDimensions": { "x": 0.2, "y": 0.2, "z": 0.2 },
    "clientOnly": false,
    "owningAvatarID": "{00000000-0000-0000-0000-000000000000}",
    // "renderInfo": null,
    "cloneable": false,
    "cloneLifetime": 300,
    "cloneLimit": 0,
    "cloneDynamic": false,
    "cloneAvatarEntity": false,
    "cloneOriginID": "{00000000-0000-0000-0000-000000000000}",
    "animation": {
        "url": "",
        "allowTranslation": true,
        "fps": 30,
        "currentFrame": 0,
        "running": false,
        "loop": true,
        "firstFrame": 0,
        "lastFrame": 100000,
        "hold": false
    },
    "jointRotationsSet": [],
    "jointRotations": [],
    "jointTranslationsSet": [],
    "jointTranslations": [],
    "relayParentJoints": false,
    "shapeType": "none",
    "compoundShapeURL": "",
    "textures": "",
    "xNNeighborID": "{00000000-0000-0000-0000-000000000000}",
    "yNNeighborID": "{00000000-0000-0000-0000-000000000000}",
    "zNNeighborID": "{00000000-0000-0000-0000-000000000000}",
    "xPNeighborID": "{00000000-0000-0000-0000-000000000000}",
    "yPNeighborID": "{00000000-0000-0000-0000-000000000000}",
    "zPNeighborID": "{00000000-0000-0000-0000-000000000000}",

};


function cleanProperties(props, removeDefaults) {

    // if there's no parent, localX properties are in world-frame.  move any of these over for consistency
    if (isNullID(props.parentID)) {
        if (props.position && !props.localPosition) {
            props.localPosition = props.position;
        }
        if (props.rotation && !props.localRotation) {
            props.localRotation = props.rotation;
        }
        if (props.velocity && !props.localVelocity) {
            props.localVelocity = props.velocity;
        }
        if (props.angularVelocity && !props.localAngularVelocity) {
            props.localAngularVelocity = props.angularVelocity;
        }
        if (props.dimensions && !props.localDimensions) {
            props.localDimensions = props.dimensions;
        }
    }

    // remove redundant and read-only properties.
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
    delete props.lifetime; // ?
    delete props.actionData;
    delete props.position;
    delete props.rotation;
    delete props.dimensions;

    // due to a quirk / bug in the exporter, these get stray red, green, blue members
    var vec3sWithStrayColorStuff = ["gravity", "registrationPoint", "localPosition", "localVelocity", "localAngularVelocity",
                                    "localDimensions", "pPosition", "velocity", "angularVelocity", "dimensions",
                                    "voxelVolumeSize"];
    for (var i = 0; i < vec3sWithStrayColorStuff.length; i++) {
        var wStrayPropName = vec3sWithStrayColorStuff[i];
        if (props.hasOwnProperty(wStrayPropName)) {
            delete props[wStrayPropName].red;
            delete props[wStrayPropName].green;
            delete props[wStrayPropName].blue;
        }
    }

    if (removeDefaults) {
        for (var key in props) {
            if (props.hasOwnProperty(key)) {
                if (objectsAlmostEqual(props[key], defaultProperties[key], 0.0003)) {
                    delete props[key];
                }
            }
        }
    }

    return props;
}


function sortPropertiesByParentChainOrder(props) {
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
        cleanProperties(entityProps, true);
        props.push(entityProps);
    }

    if (props.length === 0) {
        return null;
    }

    props = sortPropertiesByParentChainOrder(props);

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
            cleanProperties(jProps, true);
        }
    }

    for (var k = 0; k < actions.length; k++) {
        var action = actions[ k ];
        if (action.type == "offset") {
            action.pointToOffsetFrom = Mat4.transformPoint(baseMatInv, action.pointToOffsetFrom);
            delete action.pointToOffsetFrom.red;
            delete action.pointToOffsetFrom.green;
            delete action.pointToOffsetFrom.blue;
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
            cleanProperties(lst[i], true);
        }

        sortPropertiesByParentChainOrder(lst);

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
    sortPropertiesByParentChainOrder: sortPropertiesByParentChainOrder,
    entitiesIDsToProperties: entitiesIDsToProperties,
    checkRezSuccess: checkRezSuccess,
    propertiesToEntities: propertiesToEntities,
    propertiesToEntitiesAuto: propertiesToEntitiesAuto,
    getRootIDOfParentingTree: getRootIDOfParentingTree,
    getConnectedEntityIDs: getConnectedEntityIDs,
    propertySetsAreSimilar: propertySetsAreSimilar
};
