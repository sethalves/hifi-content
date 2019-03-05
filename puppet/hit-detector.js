
/* global Entities */

(function() {
    this.collisionWithEntity = function(myID, otherID, collisionInfo) {
        var otherProps = Entities.getEntityProperties(otherID, ["name"]);
        var myProps = Entities.getEntityProperties(myID, ["name"]);
        print("HIT: I am " + myProps.name + ", other is " + otherProps.name);

        Messages.sendLocalMessage("Puppet-Sword-Fight", JSON.stringify({
            method: "hit",
            myID: myID,
            otherID: otherID,
            collisionInfo: collisionInfo
        }));
    };
})
