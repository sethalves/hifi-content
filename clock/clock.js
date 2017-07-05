

(function() {

    var self = this;

    self.preload = function (entityID) {
        self.entityID = entityID;

        var userData = Entities.getEntityProperties(self.entityID, 'userData').userData;
        var data = JSON.parse(userData);
        self.hourHandID = data.hourHandID;
        self.minuteHandID = data.minuteHandID;
    }

    Script.setInterval(function () {
        var today = new Date();
        var hours = today.getUTCHours();
        var minutes = today.getUTCMinutes();

        print("UPDATING HANDS: " + hours + ", " + minutes +
              ", minuteHandID=" + self.minuteHandID + ", hourHandID=" + self.hourHandID);

        Entities.editEntity(self.hourHandID, {
            localRotation: Quat.fromPitchYawRollRadians(0, 2 * Math.PI * hours / 12, 0),
        });
        Entities.editEntity(self.minuteHandID, {
            localRotation: Quat.fromPitchYawRollRadians(0, 2 * Math.PI * minutes / 60, 0),
        });
    }, 60000); // 1 minute
});
