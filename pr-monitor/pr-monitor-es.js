"use strict";

/* global Script, Entities */

(function() {
    var self = this;

    var prUtils = Script.require(Script.resolvePath("pr-utils.js"));

    var dataCache = {};

    self.updateVisuals = function () {
        if (!self.response) {
            // error state
            Entities.editEntity(self.entityID, { color: { red: 255, green: 165, blue: 0 }});
            return;
        }
        if (self.response.mergeable) {
            self.status = "good";
        } else {
            self.status = "bad";
        }

        if (self.response.labelNames.indexOf("DO NOT MERGE") >= 0 ||
            self.response.labelNames.indexOf("QA Rejected :sob:") >= 0 ||
            self.response.labelNames.indexOf("Development Rejected") >= 0) {
            self.status = "bad";
        }

        for (var i = 0; i < self.response.statuses.length; i++) {
            var status = self.response.statuses[i];
            if (status.state == "pending") {
                self.status = "pending";
            }
        }

        if (self.previousStatus != self.status) {
            if (self.status == "good") {
                Entities.editEntity(self.entityID, { color: { red: 0, green: 255, blue: 0 }});
            } else if (self.status == "pending") {
                Entities.editEntity(self.entityID, { color: { red: 255, green: 255, blue: 0 }});
            } else {
                Entities.editEntity(self.entityID, { color: { red: 255, green: 0, blue: 0 }});
            }
            self.previousStatus = self.status;
        }

    };

    self.updateStatus = function () {
        try {
            var userData = Entities.getEntityProperties(self.entityID, "userData").userData;
            var data = JSON.parse(userData);
            self.prNumber = data.prNumber;
            prUtils.getPRDetails(self.prNumber, dataCache, function (response) {
                self.response = response;
                self.updateVisuals();
            });
        } catch (err) {
            print("pr-monitor-es.js -- updateStatus failed: " + JSON.stringify(err));
        }
    };


    self.preload = function (entityID) {
        self.entityID = entityID;

        Script.setInterval(function () {
            self.updateStatus();
        // }, 180000); // 180 second
        }, 20000); // 20 second

        self.updateStatus();
    };

});
