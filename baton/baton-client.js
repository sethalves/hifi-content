

runByOne = function(participant, batonName, maxTime, grantedThunk, deniedThunk) {
    var react;

    react = function(channel, message, sender) {
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

        if (command == "grant") {
            print("GRANTED " + batonName);
            Messages.messageReceived.disconnect(react);
            grantedThunk();
        } else if (command == "deny") {
            print("DENIED " + batonName);
            Messages.messageReceived.disconnect(react);
            deniedThunk();
        }
    }

    Messages.messageReceived.connect(react);
    Messages.subscribe("baton");
    Messages.sendMessage("baton", JSON.stringify({
        command: "claim",
        name: batonName,
        participant: participant,
        time: maxTime
    }));
}

releaseBaton = function(participant, batonName) {
    Messages.sendMessage("baton", JSON.stringify({
        command: "release",
        name: batonName,
        participant: participant
    }));
}
