"use strict";

/* global Entities, Messages, Assets, Script, print */

(function() {

    var self = this;
    var running = false;

    self.preload = function (entityID) {
        print("preload...");
        self.entityID = entityID;

        // var props = Entities.getEntityProperties(self.entityID, ["position", "rotation"]);

        // EntityViewer.setPosition(props.position);
        // EntityViewer.setOrientation(props.rotation);
        // EntityViewer.setCenterRadius(30.0);
        // EntityViewer.queryOctree();
    };

    function recreateEntities(lst, resultingPropsList, finishedCallback) {
        var props = lst.pop();

        var entityID = props.id;
        delete props.id;

        // Entities.editEntity(entityID, props);
        Entities.deleteEntity(entityID);
        var newEntityID = Entities.addEntity(props);

        props.id = newEntityID;
        resultingPropsList.push(props);

        if (lst.length > 0) {
            Script.setTimeout(function () {
                recreateEntities(lst, resultingPropsList, finishedCallback);
            }, 100);
        } else {
            running = false;
            finishedCallback(resultingPropsList);
        }
    }


    function restoreDomain() {
        if (running) {
            print("restoration is already in progress...");
            return;
        }
        running = true;


        // var props = Entities.getEntityProperties(self.entityID, ["position", "rotation"]);
        // var entityIDs = Entities.findEntities(props.position, 1000);
        // print("I see " + entityIDs.length + " entities" + ", version 5");

        print("restoring saved domain, version 9...");
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

                print("got " + savedEntityProps.length + " entities.");

                recreateEntities(savedEntityProps, [],
                                 function(resultingPropsList) {
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
        print("got message...");
        if (channel !== 'Domain-Cleaner') {
            return;
        }

        var parsedMessage = {};
        try {
            parsedMessage = JSON.parse(message);
        }  catch (e) {
            print(e);
        }

        print("got Domain-Cleaner message: " + JSON.stringify(parsedMessage));

        if (parsedMessage.action == "restore") {
            restoreDomain();
        }
    };

    Messages.messageReceived.connect(handleMessages);
    Messages.subscribe('Domain-Cleaner');

    print("starting...");
});
