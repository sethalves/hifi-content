(function () {
    Script.include("http://headache.hungry.com/~seth/hifi/baton-client.js");

    this.batonName = null;
    this.baton = null;

    this.preload = function (entityID) {
        this.entityID = entityID;
        this.turnOff();

        this.batonName = 'io.highfidelity.seth.solid-modeler:' + this.entityID;
        this.baton = acBaton({
            batonName: this.batonName,
            timeScale: 15000
        });

    }

    this.turnOn = function () {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 0 }});
        var _this = this;
        // this.baton.claim(
        //     function () { // onGrant
        //         // https://en.wikipedia.org/wiki/XMLHttpRequest
        //         var req = new XMLHttpRequest();
        //         req.responseType = 'json';
        //         req.open("POST", "http://headache.hungry.com/~seth/hifi/hifi-openscad/invoke-scad.cgi", false);
        //         // "application/json"
        //         req.send(JSON.stringify({something: "ok"}));
        //         if (req.status == 200) {
        //             print("success: " + req.response);
        //             _this.turnOff();
        //             _this.baton.release();
        //         } else {
        //             print("Error loading data: " + req.status + " " + req.statusText + ", " + req.errorCode);
        //             _this.turnOff();
        //             _this.baton.release();
        //         }
        //     });

        var req = new XMLHttpRequest();
        req.responseType = 'json';
        req.open("POST", "http://headache.hungry.com/~seth/hifi/hifi-openscad/invoke-scad.cgi", false);
        // "application/json"
        req.send(JSON.stringify({something: "ok"}));
        if (req.status == 200) {
            print("success: " + req.response);
            _this.turnOff();
            _this.baton.release();
        } else {
            print("Error loading data: " + req.status + " " + req.statusText + ", " + req.errorCode);
            _this.turnOff();
            _this.baton.release();
        }
    }

    this.turnOff = function () {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 0, red: 255 }});
    }

    this.startNearTrigger = function (entityID) {
        print("startNearTrigger calling turnOn");
        // this.entityID = entityID;
        this.turnOn();
    }

    this.stopNearTrigger = function (entityID) {
        // this.entityID = entityID;
        // this.turnOff();
    }

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        print("clickDownOnEntity calling turnOn");
        // this.entityID = entityID;
        this.turnOn();
    }

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
        // this.entityID = entityID;
        // this.turnOff();
    }
})
