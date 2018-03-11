"use strict";

/* global Entities, Script, Messages, Tablet, Assets, MyAvatar, Vec3 */

(function() { // BEGIN LOCAL_SCOPE

    var DOMAIN_CLEANER_URL = Script.resolvePath("domain-cleaner.html");

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");

    var button = tablet.addButton({
        icon: Script.resolvePath("domain-cleaner.svg"),
        text: "Cleaner",
        sortOrder: 30
    });

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

        print("got Domain-Cleaner message: " + JSON.stringify(parsedMessage));
    };

    function cleanProperties(props) {
        // delete props.id;
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
        // delete props.parentID;
        // delete props.parentJointIndex;
        // delete props.localPosition;
        // delete props.localRotation;
        delete props.lifetime;
        delete props.actionData; // XXX need to handle these differently for ID remapping
        delete props.localVelocity;
        delete props.localAngularVelocity;
        return props;
    }

    function saveDomain(params) {
        var entitiesToSave = [];
        var entityIDs = Entities.findEntities(MyAvatar.position, 1000);
        for (var j = 0; j < entityIDs.length; j++) {
            var entityID = entityIDs[j];
            var props = Entities.getEntityProperties(entityID);
            if (props.locked) {
                continue;
            }
            if (props.clientOnly) {
                continue;
            }

            props = cleanProperties(props);

            // var saveProps = {
            //     id: props.id,
            //     position: props.position,
            //     rotation: props.rotation,
            //     dimensions: props.dimensions,
            // };

            entitiesToSave.push(props);
        }

        print("found " + entitiesToSave.length + " entities.");
        var data = JSON.stringify(entitiesToSave);
        Assets.uploadData(data, function(url, hash) {
            print("save-data uploaded: " + hash);
            Assets.setMapping("/domain-cleaner-data.json", hash, function() {
                print("save-data mapping set");
            });
        });
    }

    function restoreDomain(params) {

        Messages.sendMessage('Domain-Cleaner', JSON.stringify({
            action: 'restore',
            position: MyAvatar.position
        }));

        // Assets.getMapping("/domain-cleaner-data.json", function (hash, success) {
        //     if (!success || hash === "") {
        //         print("failed to get mapping for /domain-cleaner-data.json");
        //         return;
        //     }
        //     Assets.downloadData("atp:" + hash, function (data) {
        //         var savedEntityProps = JSON.parse(data);
        //         for (var i = 0; i < savedEntityProps.length; i++) {
        //             var props = savedEntityProps[i];
        //             var entityID = props.id;
        //             delete props.id;
        //             // delete props.type;
        //             Entities.editEntity(entityID, props);
        //         }
        //     });
        // });
    }

    function deleteUnknown(params) {
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
        Messages.messageReceived.disconnect(handleMessages);
    }

    Messages.messageReceived.connect(handleMessages);

    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);
    tablet.screenChanged.connect(onScreenChanged);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
