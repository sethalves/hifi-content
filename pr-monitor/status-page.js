"use strict";

/* global Script, Entities */

(function() {
    var self = this;
    var prUtils = Script.require(Script.resolvePath("pr-utils.js?v=1"));

    self.updateStatus = function () {
        prUtils.getRateLimit(function (data) {

            var d = new Date();
            var epochTime = d.getTime() / 1000;
            var minutes = Math.round((data.resources.core.reset - epochTime) / 60.0);

            var text = "";
            text += "API Throttling\n";
            text += "\n";
            text += "" + data.resources.core.remaining + " / " + data.resources.core.limit + "\n";
            text += "\n";
            if (minutes == 1) {
                text += "" + minutes + " minute";
            } else {
                text += "" + minutes + " minutes";
            }

            Entities.editEntity(self.entityID, { text: text });
        });
    };


    self.preload = function (entityID) {
        self.entityID = entityID;

        Script.setInterval(function () {
            self.updateStatus();
        }, 20000); // 20 second

        self.updateStatus();
    };
});
