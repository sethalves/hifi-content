
"use strict";

/* global Script, Entities, Messages */

(function() {

    var self = this;
    var skyboxID = "{adff73c5-56ae-4409-a28f-53bec0606e26}";
    var skyType = 0;

    self.preload = function (entityID) {
        self.entityID = entityID;
    };

    print("cycle-sky.js starting...");


    var dayURL = "http://headache.hungry.com/~seth/hifi/trees-skybox.png";
    var twilightURL = "http://headache.hungry.com/~seth/hifi/trees-skybox-twilight.png";
    var nightURL = "http://headache.hungry.com/~seth/hifi/trees-skybox-night.png";

    var params = [
        { skyboxURL: dayURL, lightIntensity: 1.0 }, // 0
        { skyboxURL: dayURL, lightIntensity: 1.0 }, // 1
        { skyboxURL: dayURL, lightIntensity: 0.8 }, // 2
        { skyboxURL: twilightURL, lightIntensity: 0.5 }, // 3
        { skyboxURL: nightURL, lightIntensity: 0.3 }, // 4
        { skyboxURL: nightURL, lightIntensity: 0.1 }, // 5
        { skyboxURL: nightURL, lightIntensity: 0.1 }, // 6
        { skyboxURL: nightURL, lightIntensity: 0.3 }, // 7
        { skyboxURL: twilightURL, lightIntensity: 0.5 }, // 8
    ];


    function handleMessages(channel, message, sender) {
        // if (sender !== MyAvatar.sessionUUID) {
        //     return;
        // }
        if (channel !== "Day-Night-Cycle") {
            return;
        }
        var data;
        try {
            data = JSON.parse(message);
        } catch (e) {
            print("WARNING: error parsing \"Day-Night-Cycle\" message: " + message);
            return;
        }

        var method = data.method;
        if (method == "set-cycle-stage") {
            var cycleStage = data.value;
            if (cycleStage >= 0 && cycleStage < params.length) {
                var skyboxURL = params[cycleStage].skyboxURL;
                var lightIntensity = params[cycleStage].lightIntensity;

                Entities.editEntity(skyboxID, { locked: false });

                Entities.editEntity(skyboxID, {
                    ambientLight: {
                        ambientURL: skyboxURL
                    },
                    skybox: {
                        url: skyboxURL
                    },
                    keyLight: {
                        intensity: lightIntensity
                    }
                });

                Entities.editEntity(skyboxID, { locked: true });

                skyType = cycleStage; // in case some other script sent the message
            } else {
                print("WARNING: error bad \"Day-Night-Cycle\" message: " + message);
            }
        }
    }

    function cleanup() {
        Messages.unsubscribe("Day-Night-Cycle");
        Messages.messageReceived.disconnect(handleMessages);
    }


    function startup() {
        Script.scriptEnding.connect(cleanup);
        Messages.messageReceived.connect(handleMessages);
        Messages.subscribe("Day-Night-Cycle");

        Script.setInterval(function () {

            skyType++;
            if (skyType >= params.length) {
                skyType = 0;
            }

            Messages.sendMessage("Day-Night-Cycle", JSON.stringify({
                method: "set-cycle-stage",
                value: skyType
            }));

        // }, 60000); // 1 minutes
        }, 300000); // 5 minutes
    }

    startup();

});
