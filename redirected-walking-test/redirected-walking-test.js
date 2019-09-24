
/* global Script, Controller */
/* jshint loopfunc:true */

(function() {

    var mappingName = 'redirected-walking-test-' + Math.random();
    var inputMapping = Controller.newMapping(mappingName);

    function leftBlinkChanged(value) {
        print("QQQQ left blink: " + value);
    }

    function rightBlinkChanged(value) {
        print("QQQQ right blink: " + value);
    }

    function cleanUp() {
        inputMapping.disable();
    }

    Script.scriptEnding.connect(function () {
        cleanUp();
    });

    inputMapping.from(Controller.Standard.LeftEyeBlink).peek().to(leftBlinkChanged);
    inputMapping.from(Controller.Standard.RightEyeBlink).peek().to(rightBlinkChanged);
});
