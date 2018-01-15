"use strict";

/* global Entities, Script, Tablet, Assets, MyAvatar, Vec3 */

(function() { // BEGIN LOCAL_SCOPE

    var DOMAIN_CLEANER_URL = Script.resolvePath("domain-cleaner.html");

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");

    var button = tablet.addButton({
        icon: Script.resolvePath("domain-cleaner.svg"),
        text: "Cleaner",
        sortOrder: 18
    });

    function saveDomain(params) {
        var entitiesToSave = [];
        var entityIDs = Entities.findEntities(MyAvatar.position, 1000);
        for (var j = 0; j < entityIDs.length; j++) {
            var entityID = entityIDs[j];
            var props = Entities.getEntityProperties(entityID);
            if (props.locked) {
                continue;
            }
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
            delete props.localPosition;
            delete props.localRotation;
            delete props.localVelocity;
            delete props.localAngularVelocity;
            delete props.localDimensions;
            delete props.renderInfo;

            entitiesToSave.push(props);
        }

        var data = JSON.stringify(entitiesToSave);
        Assets.uploadData(data, function(url, hash) {
            Assets.setMapping("/domain-cleaner-data.json", hash, function() {
            });
        });
    }

    function restoreDomain(params) {
        Assets.getMapping("/domain-cleaner-data.json", function (hash) {
            Assets.downloadData(hash, function (data) {
                print("done downloading...");
                var savedEntityProps = JSON.parse(data);
                print("done parsing...");
                for (var i; i < savedEntityProps.length; i++) {
                    var props = savedEntityProps[i];
                    var entityID = props.id;
                    delete props.id;
                    Entities.editEntity(entityID, props);
                }
            });
        });
    }

    function deleteUnknown(params) {
        var pos = Vec3.sum(MyAvatar.position, Vec3.multiplyQbyV(MyAvatar.orientation, {x: 0, y: 0.1, z: -2}));
        Entities.addEntity({
            name: "hinge test ",
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

    function onWebEventReceived(eventString) {
        print("received web event: " + JSON.stringify(eventString));
        if (typeof eventString === "string") {
            var event;
            try {
                event = JSON.parse(eventString);
            } catch(e) {
                return;
            }

            if (event["domain-cleaner-command"]) {
                var commandToFunctionMap = {
                    "save-domain": saveDomain,
                    "restore-domain": restoreDomain,
                    "delete-unknown": deleteUnknown
                };

                var cmd = event["domain-cleaner-command"];
                if (commandToFunctionMap.hasOwnProperty(cmd)) {
                    var func = commandToFunctionMap[cmd];
                    func(event);
                }
            }
        }
    }

    var onDomainCleanerScreen = false;
    var shouldActivateButton = false;

    function onClicked() {
        if (onDomainCleanerScreen) {
            tablet.gotoHomeScreen();
        } else {
            shouldActivateButton = true;
            tablet.gotoWebScreen(DOMAIN_CLEANER_URL);
            onDomainCleanerScreen = true;
        }
    }

    function onScreenChanged() {
        // for toolbar mode: change button to active when window is first openend, false otherwise.
        button.editProperties({isActive: shouldActivateButton});
        onDomainCleanerScreen = shouldActivateButton;
        shouldActivateButton = false;
    }

    function cleanup() {
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);
    tablet.screenChanged.connect(onScreenChanged);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
