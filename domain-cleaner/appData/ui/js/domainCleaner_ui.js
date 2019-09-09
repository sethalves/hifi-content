"use strict";

/* jshint strict: true */
/* jslint vars: true */
/* global EventBridge */
/* exported buttonClicked */

var APP_NAME = "DOMAINCLEANER";

function emitAppSpecificEvent(method, data) {
    var event = {
        app: APP_NAME,
        method: method,
        data: data
    };
    EventBridge.emitWebEvent(JSON.stringify(event));
}


function buttonClicked(el) {
    switch (el.id) {
    case "save":
        console.log("ui save clicked");
        emitAppSpecificEvent("saveClicked");
        break;

    case "restore":
        console.log("ui restore clicked");
        emitAppSpecificEvent("restoreClicked");
        break;

    case "deleteUnknown":
        console.log("ui delete-others clicked");
        emitAppSpecificEvent("deleteUnknownClicked");
        break;

    default:
        console.log("Unhandled button clicked: " + el.id);
        break;
    }
}


function initializeUI(data) {
    // disable the loading spinner
    document.getElementById("loadingContainer").style.display = "none";
}


// Handle messages over the EventBridge from the App JS
function onScriptEventReceived(scriptEvent) {
    var event = scriptEvent;
    try {
        event = JSON.parse(event);
    } catch (error) {
        return;
    }

    if (event.app !== APP_NAME) {
        return;
    }

    switch (event.method) {
    case "initializeUI":
        initializeUI(event.data);
        break;

    default:
        console.log("Unrecognized event method supplied to App UI JS: " + event.method);
        break;
    }
}


// This delay is necessary to allow for the JS EventBridge to become active.
// The delay is still necessary for HTML apps in RC78+.
var EVENTBRIDGE_SETUP_DELAY = 500;
function onLoad() {
    setTimeout(function () {
        EventBridge.scriptEventReceived.connect(onScriptEventReceived);
        emitAppSpecificEvent("eventBridgeReady");
    }, EVENTBRIDGE_SETUP_DELAY);
}


// Call onLoad() once the DOM is ready
document.addEventListener("DOMContentLoaded", function (event) {
    onLoad();
});
