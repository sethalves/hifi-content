
"use strict";

/* global Script, Entities */

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
        { skyboxURL: dayURL, lightIntensity: 1.0 },
        { skyboxURL: dayURL, lightIntensity: 1.0 },
        { skyboxURL: dayURL, lightIntensity: 1.0 },
        { skyboxURL: dayURL, lightIntensity: 1.0 },
        { skyboxURL: twilightURL, lightIntensity: 0.5 },
        { skyboxURL: nightURL, lightIntensity: 0.1 },
        { skyboxURL: nightURL, lightIntensity: 0.1 },
        { skyboxURL: nightURL, lightIntensity: 0.1 },
        { skyboxURL: twilightURL, lightIntensity: 0.5 },
    ];


    Script.setInterval(function () {

        skyType++;
        if (skyType > params.length) {
            skyType = 0;
        }

        var skyboxURL = params[skyType].skyboxURL;
        var lightIntensity = params[skyType].lightIntensity;

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

    }, 300000); // 5 minutes
});
