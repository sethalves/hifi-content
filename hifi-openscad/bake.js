//
//
//

/*global Script, acBaton, Entities, Vec3, Quat */

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
            timeScale: 240000,
        });

    };

    this.turnOn = function () {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 255, red: 0 }});
        var _this = this;
        this.baton.claim(
            function () { // onGrant
                _this.findInputs();
            });
    };

    this.turnOff = function () {
        Entities.editEntity(this.entityID, { color: { blue: 0, green: 0, red: 255 }});
    };

    this.findInputs = function () {
        // figure out the center of the input area
        var inputPlatformID = this.findEntityIDByName("openscad input platform");
        if (!inputPlatformID) {
            this.turnOff();
            return;
        }
        var platformPosition = Entities.getEntityProperties(inputPlatformID, ['position']).position;
        var inputAreaCenter = Vec3.sum(platformPosition, {x:0, y:1, z:0});

        // search for input entities
        var inputs = [];
        var nearbyEntities = Entities.findEntities(inputAreaCenter, 1.5);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyProperties = Entities.getEntityProperties(nearbyID);
            if (nearbyProperties.locked) {
                continue;
            }
            if (nearbyProperties.name == "openscad button") {
                continue;
            }

            var offset = Vec3.subtract(nearbyProperties.position, inputAreaCenter);
            var rotation = Quat.safeEulerAngles(nearbyProperties.rotation);
            var dimensions = nearbyProperties.dimensions;
            var color = nearbyProperties.color;

            if (nearbyProperties.type == "Box") {
                inputs.push({
                    type: "cube",
                    translation: offset,
                    rotation: rotation,
                    scale: dimensions,
                    color: color
                });
            } else if (nearbyProperties.type == "Sphere") {
                inputs.push({
                    type: "sphere",
                    translation: offset,
                    rotation: rotation,
                    scale: dimensions,
                    color: color
                });
            }
        }

        this.invokeOpenScad(inputs, platformPosition);
    };

    this.invokeOpenScad = function (inputs, platformPosition) {
        var _this = this;
        var req = new XMLHttpRequest();
        req.responseType = 'json';
        req.open("POST", "http://headache.hungry.com/~seth/hifi/hifi-openscad/invoke-scad.cgi", false);
        req.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
        req.send(JSON.stringify(inputs));
        if (req.status == 200) {
            var keys = Object.keys(req.response);
            for (var keyIndex in keys) {
                if (keys.hasOwnProperty(keyIndex)) {
                    var key = keys[keyIndex];
                    // print("    " + key + " = " + req.response[key]);
                }
            }

            var modelURL = req.response.modelURL;
            Entities.addEntity({
                type: 'Model',
                modelURL: modelURL,
                position: Vec3.sum(platformPosition, { x: 0, y: 1.0, z: -2 }),
                userData: JSON.stringify({"grabbableKey":{"grabbable":true}})
            });
            _this.turnOff();
        } else {
            print("Error loading data: " + req.status + " " + req.statusText + ", " + req.errorCode);
            _this.turnOff();
        }
        _this.baton.release();
    };

    this.findEntityIDByName = function (entityName) {
        var myProperties = Entities.getEntityProperties(this.entityID, ['position', 'rotation']);
        var nearbyEntities = Entities.findEntities(myProperties.position, 2);
        for (var i = 0; i < nearbyEntities.length; i++) {
            var nearbyID = nearbyEntities[i];
            var nearbyName = Entities.getEntityProperties(nearbyID, ['name']).name;
            if (nearbyName == entityName) {
                return nearbyID;
            }
            // print("'" + nearbyName + "' != '" + entityName + "'");
        }
        return null;
    };

    this.startNearTrigger = function (entityID) {
        // print("startNearTrigger calling turnOn");
        // this.entityID = entityID;
        this.turnOn();
    };

    this.stopNearTrigger = function (entityID) {
        // this.entityID = entityID;
        // this.turnOff();
    };

    this.clickDownOnEntity = function (entityID, mouseEvent) {
        // print("clickDownOnEntity calling turnOn");
        // this.entityID = entityID;
        this.turnOn();
    };

    this.clickReleaseOnEntity = function (entityID, mouseEvent) {
        // this.entityID = entityID;
        // this.turnOff();
    };
})
