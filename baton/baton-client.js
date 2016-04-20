

runByOne = function(batonName, maxTime, grantedThunk, deniedThunk) {
    var react;

    print("runByOne");

    react = function(channel, message, sender) {
        if (channel != "baton") {
            return;
        }

        var messageParsed = message.split(" ");
        if (messageParsed.length < 2) {
            print("invalid message: " + message);
            return;
        }

        var command = messageParsed[0];
        var messageBatonName = messageParsed[1];

        if (batonName != messageBatonName) {
            return;
        }

        if (command == "grant") {
            if (messageParsed.length != 3) {
                print("invalid message: " + message);
                return;
            }

            var grantedTo = messageParsed[2];
            if (grantedTo == MyAvatar.sessionUUID) {
                print("GRANTED " + batonName);
                Messages.messageReceived.disconnect(react);
                grantedThunk();
            }
        } else if (command == "deny") {
            if (messageParsed.length != 3) {
                print("invalid message: " + message);
                return;
            }
            var deniedFrom = messageParsed[2];
            if (deniedFrom == MyAvatar.sessionUUID) {
                print("DENIED " + batonName);
                Messages.messageReceived.disconnect(react);
                deniedThunk();
            }
        }
    }

    Messages.messageReceived.connect(react);
    Messages.subscribe("baton");
    Messages.sendMessage("baton", "claim " + batonName + " " + maxTime);
}

releaseBaton = function(batonName) {
    Messages.sendMessage("baton", "release " + batonName);
}
