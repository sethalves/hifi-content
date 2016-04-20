//
//
//


var batons = {}; // hash key is baton-name, value is [owner, timeout] -- owner is a participant, timeout is msecs

Messages.subscribe("baton");
Messages.messageReceived.connect(function(channel, message, sender) {
    var _this = this;

    this.batonOwner = function(baton) {
        if (baton) {
            return baton[0];
        }
        return null;
    }

    this.batonTimeout = function(baton) {
        if (baton) {
            return baton[1];
        }
        return null;
    }

    this.setBaton = function(batonName, owner, timeout) {
        batons[batonName] = [owner, timeout];
    }

    this.grant = function (participant, maxTime, batonName) {
        var baton = batons[batonName];
        var owner = _this.batonOwner(baton);
        if (!owner || owner == participant) {
            print("GRANT: '" + batonName + "' to " + participant);
            // the baton will be granted (or regranted)
            owner = participant;
            // if there's an old timeout, clear it
            timeout = _this.batonTimeout(baton);
            if (timeout) {
                Script.clearTimeout(timeout);
            }
            // set new timeout
            timeout = Script.setTimeout(function() {
                print("TIMEOUT: '" + batonName + "'");
                if (batonName in batons) {
                    delete batons[batonName];
                    Messages.sendMessage("baton", JSON.stringify({
                        command: "timeout",
                        name: batonName,
                        participant: participant
                    }));
                }
            }, maxTime);
            _this.setBaton(batonName, participant, timeout);
            Messages.sendMessage("baton", JSON.stringify({
                command: "grant",
                name: batonName,
                participant: participant
            }));
        } else {
            print("DENY: '" + batonName + "' from " + participant);
            Messages.sendMessage("baton", JSON.stringify({
                command: "deny",
                name: batonName,
                participant: participant
            }));
        }
    };

    this.release = function (participant, batonName) {
        var baton = batons[batonName];
        if (!baton) {
            print(participant + " tried to release unknown baton " + batonName);
            return;
        }
        if (_this.batonOwner(baton) != participant) {
            print(participant + " tried to release baton " + batonName + ", which is owned by " + _this.batonOwner(baton));
            return;
        }
        print("RELEASE: '" + batonName + "' by " + participant);
        Script.clearTimeout(_this.batonTimeout(baton));
        delete batons[batonName];
        Messages.sendMessage("baton", JSON.stringify({
            command: "released",
            name: batonName,
            participant: participant
        }));
    };

    // main body of message receiver

    var messageParsed;
    try {
        messageParsed = JSON.parse(message);
    } catch (e) {
        print("unparsable message: " + message);
        return;
    }
    var participant = messageParsed.participant;
    var command = messageParsed.command;
    var batonName = messageParsed.name;

    if (!participant || !command || !batonName) {
        print("invalid message: " + message);
        return;
    }

    if (command == "claim") {
        var maxTime = messageParsed.time;
        if (!maxTime) {
            print("invalid claim message: " + message);
            return;
        }
        this.grant(participant, maxTime, batonName);
    } else if (command == "release") {
        this.release(participant, batonName);
    }
});
