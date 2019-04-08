"use strict";

/* global Script, Entities */

(function() {
    var self = this;

    var prUtils = Script.require(Script.resolvePath("pr-utils.js?v=2"));

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

        if (self.response.state == "closed") {
            self.status = "closed";
        }

        if (self.response.state == "merged") { // this doesn't actually work.
            self.status = "merged";
        }

        if (self.previousStatus != self.status) {
            if (self.status == "good") {
                Entities.editEntity(self.entityID, { color: { red: 0, green: 255, blue: 0 }});
            } else if (self.status == "pending") {
                Entities.editEntity(self.entityID, { color: { red: 255, green: 255, blue: 0 }});
            } else if (self.status == "closed") {
                Entities.editEntity(self.entityID, { color: { red: 120, green: 120, blue: 120 }});
            } else if (self.status == "merged") {
                Entities.editEntity(self.entityID, { color: { red: 250, green: 250, blue: 250 }});
            } else {
                Entities.editEntity(self.entityID, { color: { red: 255, green: 0, blue: 0 }});
            }
            self.previousStatus = self.status;
        }
    };


    self.updateStatus = function () {
        prUtils.getRateLimit(function (rateLimitData) {
            var d = new Date();
            var epochTime = d.getTime() / 1000;
            var seconds = rateLimitData.resources.core.reset - epochTime;
            var remainingSecondsPerRequests = seconds / rateLimitData.resources.core.remaining;

            var userData = Entities.getEntityProperties(self.entityID, "userData").userData;
            var data = JSON.parse(userData);
            self.prNumber = data.prNumber;
            prUtils.getPRDetails(self.prNumber, dataCache, function (response) {
                self.response = response;
                self.updateVisuals();

                // most requests don't end up counting against the throttling / rate-limit, because
                // they are requests for pages that haven't changed.  This slows down
                // the requests if we are using them up too quickly.
                Script.setTimeout(function () {
                    self.updateStatus();
                }, remainingSecondsPerRequests * 500.0);
            });
        });
    };


    self.preload = function (entityID) {
        self.entityID = entityID;
        self.updateStatus();
    };
});
