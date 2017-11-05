
/*global Entities, AvatarManager, MyAvatar */

(function() {

    var _this = this;

    this.preload = function(entityID) {
        this.entityID = entityID;
    };

    this.createRay = function(distance) {
        this.rayID = Entities.addEntity({
            name: "raygun beam",
            type: "Box",
            dynamic: false,
            collidesWith: "",
            collisionless: true,
            registrationPoint: { x: 0.5, y: 0.5, z: 0 },
            dimensions: { x: 0.01, y: 0.01, z: distance },
            color: { red: 0, green: 255, blue: 0 },
            parentID: this.entityID,
            parentJointIndex: -1,
            localRotation: {x: 0, y: 0, z: 0, w: 1},
            localPosition: {x: 0, y: 0.008, z: 0.12},
            lifetime: 0.08
        });
    };

    this.createHit = function (targetID, hitPoint) {
        Entities.addEntity({
            type: "Sphere",
            position: hitPoint,
            color: {red: 0, green: 200, blue: 200},
            visible: true,
            dimensions: { x: 0.6, y: 0.6, z: 0.6 },
            lifetime: 2.0
        });
    };

    this.startNearTrigger = function(hand, avatarID) {
         var pickRay = {
            origin: this.barrelPoint,
            direction: this.firingDirection
        };
        var intersection = Entities.findRayIntersection(pickRay, true);
        if (intersection.intersects) {
            _this.createHit(intersection.entityID, intersection.intersection);
            _this.createRay(intersection.distance);
        } else {
            intersection = AvatarManager.findRayIntersection(pickRay, [], [MyAvatar.sessionUUID]);
            if (intersection.intersects) {
                _this.createHit(intersection.avatarID, intersection.intersection);
                _this.createRay(intersection.distance);
            }
        }
    };

    this.stoptNearTrigger = function(hand, avatarID) {
        Entities.deleteEntity(_this.rayID);
    };

    this.cleanup = function () {
        Entities.deleteEntity(_this.rayID);
    };
});
