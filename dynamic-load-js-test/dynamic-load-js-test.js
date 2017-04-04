"use strict";

/* globals Script, Tablet */

(function() { // BEGIN LOCAL_SCOPE
    var onTestScreen = false;
    var shouldActivateButton = false;
    var DYNAMIC_LOAD_JS_TEST_URL = Script.resolvePath("dynamic-load-js-test.html");

    var tablet = Tablet.getTablet("com.highfidelity.interface.tablet.system");
    var button = tablet.addButton({
        icon: Script.resolvePath("test.svg"),
        text: "Test",
        sortOrder: 16
    });


    function runTest(params) {
        print("in runTest");
        var a = Script.require(Script.resolvePath("a.js"));
        print("a = " + a());
    }

    function reloadJS(params) {
        print("in reloadJS");
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

            if (event["dynamic-load-js-test-command"]) {
                var commandToFunctionMap = {
                    "run-test": runTest,
                    "reload-js": reloadJS
                };

                var cmd = event["dynamic-load-js-test-command"];
                if (commandToFunctionMap.hasOwnProperty(cmd)) {
                    var func = commandToFunctionMap[cmd];
                    func(event);
                }
            }
        }
    }

    function onClicked() {
        if (onTestScreen) {
            tablet.gotoHomeScreen();
        } else {
            shouldActivateButton = true;

            tablet.gotoWebScreen(DYNAMIC_LOAD_JS_TEST_URL);
            onTestScreen = true;
        }
    }

    function onScreenChanged() {
        // for toolbar mode: change button to active when window is first openend, false otherwise.
        button.editProperties({isActive: shouldActivateButton});
        shouldActivateButton = false;
        onTestScreen = shouldActivateButton;
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
