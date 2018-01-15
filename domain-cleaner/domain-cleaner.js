
"use strict";

/* global Script, Tablet */

(function() { // BEGIN LOCAL_SCOPE

    Script.include("/~/system/libraries/utils.js");

    var DOMAIN_CLEANER_UI_URL = Script.resolvePath("domain-cleaner.html");
    var EVENT_KEY = "domain-cleaner-command";


    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: Script.resolvePath("domain-cleaner.svg"),
        text: "Cleaner",
        sortOrder: 15
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


    var active = false;

    function onClicked() {
        if (active) {
            active = false;
            tablet.gotoHomeScreen();
        } else {
            active = true;
            tablet.gotoWebScreen(DOMAIN_CLEANER_UI_URL);
        }
    }

    function onScreenChanged() {
        button.editProperties({isActive: active});
    }

    function cleanup() {
        if (active) {
            active = false;
            tablet.gotoHomeScreen();
        }
        button.clicked.disconnect(onClicked);
        tablet.removeButton(button);
    }

    button.clicked.connect(onClicked);
    tablet.webEventReceived.connect(onWebEventReceived);
    tablet.screenChanged.connect(onScreenChanged);
    Script.scriptEnding.connect(cleanup);
}()); // END LOCAL_SCOPE
