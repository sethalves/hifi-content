"use strict";

/* global Entities, Messages, Assets, Script */

(function() {

    var self = this;
    var running = false;

    self.preload = function (entityID) {
        print("preload...");
        self.entityID = entityID;

        var props = Entities.getEntityProperties(self.entityID, ["position"]);

        var entityIDs = Entities.findEntities(props.position, 1000);
        print("I see " + entityIDs.length + " entities" + ", version 3");
    };

    function editEntities(lst) {
        var props = lst.pop();
        var entityID = props.id;

        delete props.id;

        Entities.editEntity(entityID, props);

        if (lst.length > 0) {
            Script.setTimeout(function () {
                editEntities(lst);
            }, 100);
        } else {
            running = false;
        }
    }


    function restoreDomain() {
        if (running) {
            print("restoration is already in progress...");
            return;
        }
        running = true;
        print("restoring saved domain...");
        Assets.getMapping("/domain-cleaner-data.json", function (hash, success) {
            if (!success || hash === "") {
                print("failed to get mapping for /domain-cleaner-data.json");
                return;
            }
            Assets.downloadData("atp:" + hash, function (data) {
                print("download of /domain-cleaner-data.json is done...");

                var savedEntityProps = JSON.parse(data);

                print("got " + savedEntityProps.length + " entities.");

                editEntities(savedEntityProps);
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
