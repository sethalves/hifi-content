
/* global Entities, SoundCache, Script, Messages */

(function() {

    var sound = SoundCache.getSound(Script.resourcesPath() + "sounds/short1.wav");
    var injector;

    this.collisionWithEntity = function(myID, otherID, collisionInfo) {
        // var otherProps = Entities.getEntityProperties(otherID, ["name"]);
        // var myProps = Entities.getEntityProperties(myID, ["name"]);
        // print("HIT: I am " + myProps.name + ", other is " + otherProps.name);

        if (collisionInfo.type == 0) { // 0 = start, 1 = continue, 2 = end
            var injectorOptions = {
                position: collisionInfo.contactPoint
            };
            injector = Audio.playSound(sound, injectorOptions);

            Messages.sendLocalMessage("Puppet-Sword-Fight", JSON.stringify({
                method: "hit",
                myID: myID,
                otherID: otherID,
                collisionInfo: collisionInfo
            }));
        }
    };
})
