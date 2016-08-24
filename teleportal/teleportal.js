/* global MyAvatar, Vec3, Script, getEntityCustomData */
(function(){
    Script.include("http://headache.hungry.com/~seth/hifi/utils.js");
    this.teleportAvatar = function(teleportalZoneID) {
        var destination = getEntityCustomData('teleportal-destination', teleportalZoneID, null);
        if (destination) {
            Window.location = destination;
            MyAvatar.addThrust(Vec3.multiply(MyAvatar.velocity, -1.0)); // negate velocity
        }
    };

    this.enterEntity = function(teleportalZoneID) {
        this.teleportAvatar(teleportalZoneID);
    };
})
