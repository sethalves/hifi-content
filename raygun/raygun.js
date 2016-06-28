
(function() {
    var _this;
    Script.include("http://headache.hungry.com/~seth/hifi/hcEdit/genericTool.js");

    var rayGun;

    rayGun = genericTool(
        // start
        function() {
            var props = Entities.getEntityProperties(this.getEntityID());
            this.createRay();
        },
        // continue
        function() {
            this.updateRay();
        },
        // stop
        function() {
            Entities.deleteEntity(this.rayID);
        }
    );

    rayGun.createRay = function() {
        this.rayCreatedTime = Date.now();
        this.rayID = Entities.addEntity({
            name: "raygun beam",
            type: "Box",
            dynamic: false,
            collidesWith: "",
            collisionless: true,
            registrationPoint: { x: 0.5, y: 0.5, z: 0 },
            dimensions: { x: 0.01, y: 0.01, z: 20 },
            color: { red: 0, green: 255, blue: 0 },
            parentID: this.getEntityID(),
            parentJointIndex: -1,
            localRotation: {x: 0, y: 0, z: 0, w: 1},
            localPosition: {x: 0, y: 0.008, z: 0.12},
            lifetime: 0.1
        });
    };

    rayGun.updateRay = function() {
        // var now = Date.now();
        // if (now - this.rayCreatedTime > 0.5 * 1000) { // 0.5 seconds
        //     var age = Entities.getEntityProperties(this.rayID, "age").age;
        //     Entities.editEntity(this.rayID, { lifetime: age + 5 });
        // }
    };

    return rayGun;
});
