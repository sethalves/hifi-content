
"use strict";

/* global Script, Tablet */

(function() { // BEGIN LOCAL_SCOPE

    var DOMAIN_CLEANER_UI_URL = Script.resolvePath("domain-cleaner.html");
    var EVENT_KEY = "domain-cleaner-command";

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");

    var button = tablet.addButton({
        icon: Script.resolvePath("domain-cleaner.svg"),
        text: "Cleaner",
        sortOrder: 18
    });

    function saveDomain(params) {
        print("SAVE");
    }

    function restoreDomain(params) {
        print("RESTORE");
    }

    function deleteUnknown(params) {
        print("DELETE");
    }

    function onWebEventReceived(eventString) {
        print("domain-cleaner received web event: " + JSON.stringify(eventString));
        if (typeof eventString === "string") {
            var event;
            try {
                event = JSON.parse(eventString);
            } catch(e) {
                return;
            }

            if (event[EVENT_KEY]) {
                var commandToFunctionMap = {
                    "save": saveDomain,
                    "restore": restoreDomain,
                    "delete": deleteUnknown
                };

                var cmd = event[EVENT_KEY];
                if (commandToFunctionMap.hasOwnProperty(cmd)) {
                    var func = commandToFunctionMap[cmd];
                    func(event);
                }
            }
        }
    }

    var onCleanupScreen = false;
    var shouldActivateButton = false;

    function onClicked() {
        if (onCleanupScreen) {
            tablet.gotoHomeScreen();
        } else {
            shouldActivateButton = true;
            tablet.gotoWebScreen(DOMAIN_CLEANER_UI_URL);
            onCleanupScreen = true;
        }
    }

    function onScreenChanged() {
        // for toolbar mode: change button to active when window is first openend, false otherwise.
        button.editProperties({isActive: shouldActivateButton});
        shouldActivateButton = false;
        onCleanupScreen = shouldActivateButton;
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
