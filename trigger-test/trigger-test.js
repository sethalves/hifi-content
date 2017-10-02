/* global Entities, Script */

(function () {

    function pulseLight(lightID) {
        Entities.editEntity(lightID, { color: { blue: 0, green: 255, red: 0 }});
        Script.setTimeout(function () {
            Entities.editEntity(lightID, { color: { blue: 0, green: 0, red: 255 }});
        }, 100);
    }

    this.preload = function (entityID) {
        this.entityID = entityID;
        var userData = JSON.parse(Entities.getEntityProperties(this.entityID, ["userData"]).userData);
        this.nearLights = userData.triggerTest.near;
        this.farLights = userData.triggerTest.far;
        this.mouseLights = userData.triggerTest.mouse;
    };

    this.startNearTrigger = function (entityID) {
        pulseLight(this.nearLights[0]);
    };

    this.continueNearTrigger = function (entityID) {
        pulseLight(this.nearLights[1]);
    };

    this.stopNearTrigger = function (entityID) {
        pulseLight(this.nearLights[2]);
    };

    this.startFarTrigger = function (entityID) {
        pulseLight(this.farLights[0]);
    };

    this.continueFarTrigger = function (entityID) {
        pulseLight(this.farLights[1]);
    };

    this.stopFarTrigger = function (entityID) {
        pulseLight(this.farLights[2]);
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        pulseLight(this.mouseLights[0]);
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
        pulseLight(this.mouseLights[2]);
    };
});
