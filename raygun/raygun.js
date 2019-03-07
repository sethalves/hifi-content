
/*global Entities, Script, Vec3, Messages */

(function() {
    var genericTool = Script.require("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");
    var freezeEffect = Script.require("http://headache.hungry.com/~seth/hifi/freeze-effect/freeze-effect.js");

    var rayGun;
    var freezeTime = 5.0;

    rayGun = genericTool.genericTool(
        // start
        function() {
            var origin = this.pickRay.origin;
            var direction = Vec3.normalize(this.pickRay.direction);
            var distance = -1;
            var hitCoords;
            if (this.targetAvatar) {
                hitCoords = Vec3.sum(origin, Vec3.multiply(this.avatarDistance, direction));
                distance = this.avatarDistance;
                freezeEffect.freezeAvatar(this.targetAvatar, freezeTime);
            }
            if (this.targetEntity && (distance < 0 || this.entityDistance < distance)) {
                hitCoords = Vec3.sum(origin, Vec3.multiply(this.entityDistance, direction));
                distance = this.entityDistance;
            }

            if (distance > 0) {
                Entities.addEntity({
                    type: "Sphere",
                    position: hitCoords,
                    color: {red: 0, green: 200, blue: 200},
                    visible: true,
                    dimensions: { x: 0.6, y: 0.6, z: 0.6 },
                    lifetime: 2.0,
                    alpha: 0.6
                });
            } else {
                distance = 20;
            }

            this.createRay(distance);
        },
        // continue
        function() {
            this.updateRay();
        },
        // stop
        function() {
            // Entities.deleteEntity(this.rayID);
        }
    );

    rayGun.createRay = function(distance) {
        var children = Entities.getChildrenIDs(this.getEntityID());
        children.forEach(function(childID) {
            Entities.deleteEntity(childID);
        });

        this.rayCreatedTime = Date.now();
        this.rayID = Entities.addEntity({
            name: "raygun beam",
            type: "Box",
            dynamic: false,
            collidesWith: "",
            collisionless: true,
            registrationPoint: { x: 0.5, y: 0.5, z: 0 },
            dimensions: { x: 0.01, y: 0.01, z: distance },
            color: { red: 0, green: 255, blue: 0 },
            parentID: this.getEntityID(),
            parentJointIndex: -1,
            localRotation: {x: 0, y: 0, z: 0, w: 1},
            localPosition: {x: 0, y: 0.008, z: 0.12},
            lifetime: 0.25
        });
    };

    rayGun.updateRay = function() {
        // var now = Date.now();
        // if (now - this.rayCreatedTime > 0.5 * 1000) { // 0.5 seconds
        //     var age = Entities.getEntityProperties(this.rayID, "age").age;
        //     Entities.editEntity(this.rayID, { lifetime: age + 5 });
        // }
    };

    rayGun.laserOffsets = { x: 0, y: 0.008, z: 0.12 };

    rayGun.handleMessages = function (channel, message, sender) {
        if (channel !== "Freeze-Avatar") {
            return;
        }
        var data;
        try {
            data = JSON.parse(message);
        } catch (e) {
            print("WARNING: error parsing 'Freeze-Avatar' message: " + message);
            return;
        }

        if (data.method == "freeze" && data.targetID == rayGun.equipperID) {
            rayGun.canActivate = false;
        } else if (data.method == "unfreeze" && data.targetID == rayGun.equipperID) {
            rayGun.canActivate = true;
        }
    };

    Messages.messageReceived.connect(function (channel, message, sender) {
        rayGun.handleMessages(channel, message, sender);
    });
    Messages.subscribe("Freeze-Avatar");

    return rayGun;
});
