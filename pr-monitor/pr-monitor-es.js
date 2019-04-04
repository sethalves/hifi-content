"use strict";

/* global Script, Entities */

(function() {
    var self = this;

    var prUtils = Script.require(Script.resolvePath("pr-utils.js"));

    self.updateVisuals = function () {

        if (self.response.mergeable) {
            self.status = "good";
        } else {
            self.status = "bad";
        }

        if (self.response.labelNames.indexOf("DO NOT MERGE") >= 0) {
            self.status = "bad";
        }

        if (self.previousStatus != self.status) {
            if (self.status == "good") {
                Entities.editEntity(self.entityID, { color: { red: 0, green: 255, blue: 0 }});
            } else {
                Entities.editEntity(self.entityID, { color: { red: 255, green: 0, blue: 0 }});
            }
            self.previousStatus = self.status;
        }

    };

    self.updateStatus = function () {

        try {
            print("pr-monitor: updating...");

            var userData = Entities.getEntityProperties(self.entityID, "userData").userData;
            print("pr-monitor: userData = " + JSON.stringify(userData));
            var data = JSON.parse(userData);
            self.prNumber = data.prNumber;

            print("pr-monitor: pr number is " + self.prNumber);

            prUtils.getPRDetails(self.prNumber, function (response) {
                self.response = response;
                self.updateVisuals();
            });
        } catch (err) {
        }
    };


    self.preload = function (entityID) {
        self.entityID = entityID;

        print("pr-monitor: starting");

        Script.setInterval(function () {
            self.updateStatus();
        }, 100000); // 100 second

        self.updateStatus();
    };

});
