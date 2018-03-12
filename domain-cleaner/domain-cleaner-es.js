"use strict";

/* global Entities, Messages, Assets, Script, print */

(function() {

    var self = this;
    var running = false;
    var beenHereRetries = 10;

    self.preload = function (entityID) {
        print("preload...");
        self.entityID = entityID;

        // var props = Entities.getEntityProperties(self.entityID, ["position", "rotation"]);

        // EntityViewer.setPosition(props.position);
        // EntityViewer.setOrientation(props.rotation);
        // EntityViewer.setCenterRadius(30.0);
        // EntityViewer.queryOctree();
    };

    function recurseRecreateEntities(initialPropsList, resultingPropsList, idMapping, finishedCallback) {
        if (initialPropsList.length > 0) {
            Script.setTimeout(function () {
                recreateEntities(initialPropsList, resultingPropsList, idMapping, finishedCallback);
            }, 100);
        } else {
            running = false;
            finishedCallback(resultingPropsList);
        }
    }

    function recreateEntities(initialPropsList, resultingPropsList, idMapping, finishedCallback) {
        var props = initialPropsList.pop();

        // rez parents before children
        if (props.parentID &&
            props.parentID != "{00000000-0000-0000-0000-000000000000}" &&
            !idMapping.hasOwnProperty(props.parentID)) {
            if (props.beenHere && props.beenHere > beenHereRetries) {
                // this entity was already put to the back of the line, and it's here again too many times; give up on it.
                resultingPropsList.push(props);
            } else {
                if (props.beenHere) {
                    props.beenHere += 1;
                } else {
                    props.beenHere = 1;
                }
                // this entity refers to another entity which hasn't yet been rezzed. put it at the back of the line.
                initialPropsList.unshift(props);
            }
            recurseRecreateEntities(initialPropsList, resultingPropsList, idMapping, finishedCallback);
            return;
        }

        delete props.beenHere;

        var entityID = props.id;
        delete props.id;

        var actions = props.actions;
        delete props.actions;

        // Entities.editEntity(entityID, props);
        Entities.deleteEntity(entityID);
        var newEntityID = Entities.addEntity(props);

        props.id = newEntityID;
        props.actions = actions; // actions are re-added later, once everything is rezzed
        resultingPropsList.push(props);

        recurseRecreateEntities(initialPropsList, resultingPropsList, idMapping, finishedCallback);
    }

    function reAddActions(resultingPropsList, idMapping) {
        // for each entity...
        for (var i = 0; i < resultingPropsList.length; i++) {
            var props = resultingPropsList[i];
            if (props.actions) {
                // for each action on this entity...
                for (var j = 0; j < props.actions.length; j++) {
                    var action = props.actions[j];
                    // if the action referes to some other entity that may have been deleted and recreated,
                    // update the other entity's ID in the action arguments.
                    if (action.otherEntityID &&
                        action.otherEntityID != "{00000000-0000-0000-0000-000000000000}" &&
                        idMapping.hasOwnProperty(action.otherEntityID)) {
                        action.otherEntityID = idMapping[action.otherEntityID];
                    }
                    var actionType = action.type;
                    delete action.type;
                    Entities.addAction(actionType, props.id, action);
                }
            }
        }
    }

    function restoreDomain() {
        if (running) {
            print("domain restoration is already in progress...");
            return;
        }
        running = true;


        // var props = Entities.getEntityProperties(self.entityID, ["position", "rotation"]);
        // var entityIDs = Entities.findEntities(props.position, 1000);
        // print("I see " + entityIDs.length + " entities" + ", version 5");

        print("restoring saved domain, version 10...");
        Assets.getMapping("/domain-cleaner-data.json", function (error, hash) {
            print("Assets.getMapping callback: " + error + " " + hash);
            if (error || hash === "") {
                print("failed to get mapping for /domain-cleaner-data.json -- " + error);
                return;
            }
            print("calling Assets.downloadData");
            Assets.downloadData("atp:" + hash, function (data) {
                print("download of /domain-cleaner-data.json is done...");

                var savedEntityProps = JSON.parse(data);
                var idMapping = {}; // key is previous entityID, value is new entityID

                print("got " + savedEntityProps.length + " entities.");

                recreateEntities(savedEntityProps, [], idMapping,
                                 function(resultingPropsList) {
                                     // re-add actions
                                     reAddActions(resultingPropsList, idMapping);
                                     // upload the new list of entities
                                     print("resaving " + resultingPropsList.length + " entities");
                                     var newData = JSON.stringify(resultingPropsList);
                                     Assets.uploadData(newData, function(url, hash) {
                                         print("save-data uploaded: " + hash);
                                         Assets.setMapping("/domain-cleaner-data.json", hash, function() {
                                             print("save-data mapping set");
                                         });
                                     });
                                 });
            });
        });
    }

    var handleMessages = function(channel, message, sender) {
        if (channel !== 'Domain-Cleaner') {
            return;
        }

        var parsedMessage = {};
        try {
            parsedMessage = JSON.parse(message);
        }  catch (e) {
            print(e);
        }

        print("got Domain-Cleaner [12] message: " + JSON.stringify(parsedMessage));

        if (parsedMessage.action == "restore") {
            restoreDomain();
        }
    };

    Messages.messageReceived.connect(handleMessages);
    Messages.subscribe('Domain-Cleaner');

    print("domain-cleaner-es starting...");
});
