"use strict";

/* global Script, EntityScriptFilter, print */

(function() {
    var AppUi = Script.require("appUi");
    var ui;

    function EntityScriptBlocker() {

        this.initUI = function () {

            print("QQQQ initUI");
            ui.sendMessage({ method: "enabled", value: this.enabled });
            ui.sendMessage({ method: "clear" });
            ui.sendMessage({ method: "whitelist", value: EntityScriptFilter.getEntityScriptURLsInWhitelist() });
            ui.sendMessage({ method: "greylist", value: EntityScriptFilter.getEntityScriptURLsInPurgatory() });
            ui.sendMessage({ method: "blacklist", value: EntityScriptFilter.getEntityScriptURLsInBlacklist() });
            ui.sendMessage({ method: "sync" });
        };

        this.setEnabled = function (value) {
            this.enabled = value;
        };

        this.cleanup = function () {
        };
    }

    var entityScriptBlocker = new EntityScriptBlocker();

    function cleanup() {
        entityScriptBlocker.cleanup();
    }

    function initUI() {
        entityScriptBlocker.initUI();
    }

    ui = new AppUi({
        buttonName: "Entity Scripts",
        home: Script.resolvePath("entity-script-blocker.qml"),
        onMessage: fromQml,
        onOpened: initUI
        // normalButton: "icons/tablet-icons/avatar-i.svg",
        // activeButton: "icons/tablet-icons/avatar-a.svg",
    });

    Script.scriptEnding.connect(cleanup);

    function fromQml(message) {
        print("message from qml: " + JSON.stringify(message));
        if (message.method == "enabled") { entityScriptBlocker.setEnabled(message.value); }
    }

}());
