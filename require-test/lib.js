/* global libContents:true, module */

function libContents(fuh) {
    Script.include("/~/system/libraries/utils.js");

    var Tool = function () {

        this.activateSound =
            SoundCache.getSound("https://hifi-public.s3.amazonaws.com/sounds/Switches%20and%20sliders/lamp_switch_2.wav");

    };

    Tool.prototype = {

        // fuh: fuh,

        doSomething: function () {
            // print("QQQQ something: " + this.fuh);
            print("QQQQ something: " + fuh);
        }

    };

    return new Tool();
}


module.exports = {
    libContents: libContents
};
