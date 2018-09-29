(function () {

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.turnOn = function () {
        Entities.editEntity(this.entityID, { locked: false });
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 0 }});
        Entities.editEntity(this.entityID, { locked: true });
    };

    this.turnOff = function () {
        Entities.editEntity(this.entityID, { locked: false });
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 0, red: 255 }});
        Entities.editEntity(this.entityID, { locked: true });
    };

    this.startNearTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.stopNearTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
        this.entityID = entityID;
        this.turnOff();
    };

    this.startFarTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOn();
    };

    this.stopFarTrigger = function (entityID) {
        this.entityID = entityID;
        this.turnOff();
    };
})
