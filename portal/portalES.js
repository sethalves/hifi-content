/* global Script, Entities */
(function(){
    Script.include("http://headache.hungry.com/~seth/hifi/utils.js");
    this.teleportAvatar = function(teleportalZoneID) {
        var userData = Entities.getEntityProperties(teleportalZoneID, ["userData"]).userData;
        try {
            var userDataParsed = JSON.parse(userData);
            var destination = userDataParsed["teleportal-destination"];
            if (destination) {
                Window.location = destination;
            }
        } catch (err) {
        }
    };

    this.enterEntity = function(teleportalZoneID) {
        this.teleportAvatar(teleportalZoneID);
    };
})
