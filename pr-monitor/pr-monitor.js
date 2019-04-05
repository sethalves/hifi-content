"use strict";

/* global Entities, Script, Vec3 */

(function() {

    var prUtils = Script.require(Script.resolvePath("pr-utils.js"));
    var self = this;
    var dataCache = {};

    function updateStatus(thunk) {
        try {
            var userData = Entities.getEntityProperties(self.entityID, "userData").userData;
            var data = JSON.parse(userData);
            self.prNumber = data.prNumber;
            prUtils.getPRDetails(self.prNumber, dataCache, thunk);
        } catch (err) {
        }
    }

    function hideDetails() {
        if (self.prDetailsOverlayID) {
            Entities.deleteEntity(self.prDetailsOverlayID);
            self.prDetailsOverlayID = null;
        }
    }


    function showDetails() {
        hideDetails();

        var entityPosition = Entities.getEntityProperties(self.entityID, ["position"]).position;
        self.prDetailsOverlayID = Entities.addEntity({
            type: "Text",
            name: "PR Status ",
            position: Vec3.sum(entityPosition, { x: 0.0, y: 0.26, z: 0.0 }),
            localOrientation: { x: 0.0, y: 0.0, z: 0.0, w: 1.0 },
            text: "",
            textAlpha: 1,
            textColor: { red: 255, green: 255, blue: 255 },
            backgroundAlpha: 1,
            backgroundColor: { red: 0, green: 0, blue: 0 },
            lineHeight: 0.042,
            billboardMode: "full",
            dimensions: { x: 0.5, y: 0.5 },
            visible: true,
            ignoreRayIntersection: true,
            drawInFront: true,
            grabbable: false,
            parentID: self.entityID,
            lifetime: 100
        }, "local");

        updateStatus(function (response) {
            self.response = response;

            var prDetailsText = ".    \n";
            prDetailsText += "    PR-" + self.prNumber + "\n";
            if (self.response.milestone) {
                prDetailsText += "    milestone: " + self.response.milestone.title + "\n";
            }
            prDetailsText += "    state: " + self.response.state + "\n";
            prDetailsText += "    title: " + self.response.title + "\n";
            prDetailsText += "    ----------" + "\n    ";
            prDetailsText += self.response.labelNames.join("\n    ");

            Entities.editEntity(self.prDetailsOverlayID, {
                name: "PR Status " + self.response.number,
                text: prDetailsText
            });
        });
    }

    self.preload = function (entityID) {
        self.entityID = entityID;
    };

    self.startNearGrab = function (id, params) {
        showDetails();
    };
    self.startDistanceGrab = function (id, params) {
        showDetails();
    };
    self.releaseGrab = function (id, params) {
        hideDetails();
    };

});
