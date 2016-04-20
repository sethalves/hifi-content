//
//
//


var NULL_ID = "{00000000-0000-0000-000000000000}";

var owners = {};
var timeouts = {};


Messages.subscribe("baton");
Messages.messageReceived.connect(function(channel, message, sender) {
    var _this = this;
    // this.owners = {};
    // this.timeouts = {};

    this.grant = function (sender, maxTime, batonName) {
        print("GRANT: '" + batonName + "'");

        if (!(batonName in owners)) {
            // a baton name we haven't seen before
            owners[batonName] = NULL_ID;
            timeouts[batonName] = null;
        }

        if (owners[batonName] == NULL_ID || owners[batonName] == sender) {
            // the baton will be granted (or regranted)
            owners[batonName] = sender;
            // if there's an old timeout, clear it
            if (timeouts[batonName]) {
                Script.clearTimeout(timeouts[batonName]);
            }
            // set new timeout
            timeouts[batonName] = Script.setTimeout(function() {
                print("TIMEOUT: '" + batonName + "'");
                if (owners[batonName] != NULL_ID) {
                    Messages.sendMessage("baton", "timeout " + batonName + " " + owners[batonName]);
                    owners[batonName] = NULL_ID;
                }
            }, maxTime);
            Messages.sendMessage("baton", "grant " + batonName + " " + owners[batonName]);
        } else {
            Messages.sendMessage("baton", "deny " + batonName + " " + owners[batonName]);
        }
    };

    this.release = function (sender, batonName) {
        print("RELEASE: '" + batonName + "'");
        if (!(batonName in owners)) {
            print(sender + " tried to release unknown baton " + batonName);
            return;
        }
        if (owners[batonName] != sender) {
            print(sender + " tried to release baton " + batonName + ", which is owned by " + owners[batonName]);
            return;
        }
        if (timeouts[batonName]) {
            print("clearing timeout");
            Script.clearTimeout(timeouts[batonName]);
        }
        owners[batonName] = NULL_ID;
        timeouts[batonName] = null;
        Messages.sendMessage("baton", "released " + batonName + " " + owners[batonName]);
    };

    // main body of message receiver

    var messageParsed = message.split(" ");
    if (messageParsed.length < 2) {
        print("invalid message: " + message);
        return;
    }
    var command = messageParsed[0];
    var batonName = messageParsed[1];

    if (command == "claim") {
        if (messageParsed.length != 3) {
            print("invalid claim message: " + message);
            return;
        }
        var maxTime = messageParsed[2];
        _this.grant(sender, maxTime, batonName);
    } else if (command == "release") {
        if (messageParsed.length != 2) {
            print("invalid release message: " + message);
            return;
        }
        _this.release(sender, batonName);
    }
});
