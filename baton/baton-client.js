//
//
//

acBaton = function (options) {
    var _this = this;
    this.onGrant = null;
    this.onRelease = null;
    this.onDenied = null;

    var batonName = options.batonName || "unknown",
        participant = options.instanceId || MyAvatar.sessionUUID,
        timeScale = options.timeScale || 1000, // ms
        serverResponseTimeout = options.serverTimeOut || 2000, // ms
        exports = options.exports || {};

    exports.claim = function claim(onGrant, onRelease, onDenied, onNoServerResponse) {
        _this.onGrant = onGrant;
        _this.onRelease = onRelease;
        _this.onDenied = onDenied;
        _this.onNoServerResponse = onNoServerResponse;

        Messages.sendMessage("baton", JSON.stringify({
            command: "claim",
            name: batonName,
            participant: participant,
            time: timeScale
        }));

        _this.responseTimeout = Script.setTimeout(function() {
            // no response from server.  just go ahead
            print("NO RESPONSE " + batonName);
            if (_this.onNoServerResponse) {
                _this.onNoServerResponse();
            }
        }, serverResponseTimeout);
        return exports;
    };

    exports.release = function release() {
        Messages.sendMessage("baton", JSON.stringify({
            command: "release",
            name: batonName,
            participant: participant
        }));
        return exports;
    };

    var messageHandler = function(channel, message, sender) {
        if (channel != "baton") {
            return;
        }

        var messageParsed;
        try {
            messageParsed = JSON.parse(message);
        } catch (e) {
            print("unparsable message: " + message);
            return;
        }

        var command = messageParsed.command;
        var messageBatonName = messageParsed.name;
        var messageParticipant = messageParsed.participant;

        if (!command || !messageBatonName || !messageParticipant) {
            print("invalid message: " + message);
            return;
        }
        if (batonName != messageBatonName) {
            return;
        }
        if (participant != messageParticipant) {
            return;
        }

        Script.clearTimeout(_this.responseTimeout);

        if (command == "grant") {
            print("GRANTED " + batonName);
            if (_this.onGrant) {
                _this.onGrant();
            }
        } else if (command == "deny") {
            print("DENIED " + batonName);
            if (_this.onDenied) {
                _this.onDenied();
            }
        } else if (command == "release") {
            print("RELEASED " + batonName);
            if (_this.onRelease) {
                _this.onRelease();
            }
        }
    };

    exports.unload = function unload() { // Disconnect from everything.
        messages.messageReceived.disconnect(messageHandler);
        return exports;
    };

    Messages.subscribe("baton");
    Messages.messageReceived.connect(messageHandler);

    return exports;
}
