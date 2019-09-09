"use strict";

// Distributed under the Apache License, Version 2.0
// See the accompanying file LICENSE or http://www.apache.org/licenses/LICENSE-2.0.html

/* jshint strict: true */
/* jslint vars: true */
/* global Entities, Script, Assets, MyAvatar, Vec3 */

(function() { // BEGIN LOCAL_SCOPE

    // This is the message that's received from the UI JS that indicates that the UI is ready.
    function onEventBridgeReady() {
        ui.sendMessage({
            app: APP_NAME,
            method: "initializeUI",
            data: {}
        });
    }

    // Handle EventBridge messages from UI JavaScript.
    function onWebEventReceived(event) {

        if (event.app !== APP_NAME) {
            return;
        }

        switch (event.method) {
        case "eventBridgeReady":
            onEventBridgeReady();
            break;

        case "saveClicked":
            // saveDomain();
            break;

        case "restoreClicked":
            // restoreDomain();
            break;

        case "deleteUnknownClicked":
            // deleteUnknown();
            break;

        default:
            console.log("Unrecognized event method supplied to App JS: " + event.method);
            break;
        }
    }


    var AppUi = Script.require("appUi");
    var APP_NAME = "DOMAINCLEANER";
    var ui;

    var EUs = Script.require("http://headache.hungry.com/~seth/hifi/entity-utils/entity-utils.js");
    var entitiesIDsToProperties = EUs.entitiesIDsToProperties;
    var propertiesToEntities = EUs.propertiesToEntities;
    var getIDsFromProperties = EUs.getIDsFromProperties;
    var deleteEntities = EUs.deleteEntities;

    var saveIsRunning = false;
    var restoreIsRunning = false;


    function saveEntities(entityIDsToSave) {
        print("found " + entityIDsToSave.length + " entities.");
        var savedEntitiesProps = entitiesIDsToProperties(entityIDsToSave, { x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 0, w: 1 });

        var data = JSON.stringify(savedEntitiesProps);
        Assets.uploadData(data, function(url, hash) {
            print("save-data uploaded: " + hash);
            Assets.setMapping("/domain-cleaner-data.json", hash, function() {
                print("save-data mapping set");
            });
        });
    }


    function saveDomain() {
        if (saveIsRunning) {
            print("domain save is already in progress...");
            return;
        }
        saveIsRunning = true;

        var entityIDs = Entities.findEntities(MyAvatar.position, 1000);
        var entityIDsToSave = [];

        for (var j = 0; j < entityIDs.length; j++) {
            var entityID = entityIDs[j];
            var props = Entities.getEntityProperties(entityID, ["entityHostType", "locked"]);
            if (props.locked) {
                continue;
            }
            if (props.entityHostType != "domain") {
                continue;
            }
            entityIDsToSave.push(entityID);
        }

        saveEntities(entityIDsToSave);
        saveIsRunning = false;
    }


    function restoreDomain() {
        if (restoreIsRunning) {
            print("domain restoration is already in progress...");
            return;
        }
        restoreIsRunning = true;

        print("Restoring saved domain...");
        Assets.getMapping("/domain-cleaner-data.json", function (error, hash) {
            print("Assets.getMapping callback: " + error + " " + hash);
            if (error || hash === "") {
                print("failed to get mapping for /domain-cleaner-data.json -- " + error);
                restoreIsRunning = false;
                return;
            }
            print("Calling Assets.downloadData...");
            Assets.downloadData("atp:" + hash, function (data) {
                print("Download of /domain-cleaner-data.json is done.");

                var savedEntitiesProps = JSON.parse(data);

                var oldEntityIDs = getIDsFromProperties(savedEntitiesProps);
                print("Deleting IDs: " + JSON.stringify(oldEntityIDs));
                deleteEntities(oldEntityIDs, 30, function () {
                    print("Done deleting.");
                    var newEntityIDs = propertiesToEntities(savedEntitiesProps,
                                                            { x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 0, w: 1 }, false);
                    print("Resaving...");
                    saveEntities(newEntityIDs);
                    print("Done Resaving.");
                    restoreIsRunning = false;
                });
            });
        });
    }


    function deleteUnknown() {
        var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.1, z: -2}));
        Entities.addEntity({
            name: "domain-cleaner test",
            type: "Box",
            color: { blue: 200, green: 0, red: 0 },
            dimensions: { x: 0.2, y: 0.2, z: 0.1 },
            position: Vec3.sum(pos, {x: 0, y: 1, z:0}),
            dynamic: false,
            collisionless: true,
            lifetime: 120,
            gravity: { x: 0, y: 0, z: 0 }
        });
    }


    function cleanup() {
        if (ui.isOpen) {
            ui.onClosed();
        }
    }

    Script.scriptEnding.connect(cleanup);


    function startup() {
        ui = new AppUi({
            buttonName: "CLEANER",
            home: Script.resolvePath("./ui/domainCleaner_ui.html"),
            onMessage: onWebEventReceived,
            normalButton: Script.resolvePath("domain-clean.svg"),
            activeButton: Script.resolvePath("domain-clean.svg")
        });

        Script.scriptEnding.connect(cleanup);
    }

    startup();

}()); // END LOCAL_SCOPE
