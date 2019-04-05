"use strict";

/* global Script, Entities */

(function() {
    var self = this;
    var prUtils = Script.require(Script.resolvePath("pr-utils.js"));

    self.updateStatus = function () {
        prUtils.getRateLimit(function (data) {

            print("Here: " + JSON.stringify(data));

            var d = new Date();
            var epochTime = d.getTime() / 1000;
            var minutes = Math.round((data.resources.core.reset - epochTime) / 60.0);

            var text = ".    \n";
            text += "    Github API Throttling";
            text += "    \n";
            text += "    " + data.resources.core.remaining + " / " + data.resources.core.limit + "\n";
            text += "    \n";
            text += "    " + minutes + " minutes";

            print("text = " + JSON.stringify(text));

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
