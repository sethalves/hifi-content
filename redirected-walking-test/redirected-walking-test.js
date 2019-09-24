
/* global Script, Controller */
/* jshint loopfunc:true */

(function() {

    var mappingName = 'redirected-walking-test-' + Math.random();
    var inputMapping = Controller.newMapping(mappingName);

    var leftClosed = false;
    var rightClosed = false;

    var rightTurn = Quat.fromPitchYawRollDegrees(0, 4.5, 0);

    function go() {
        print("blink");
        MyAvatar.orientation = Quat.multiply(MyAvatar.orientation, rightTurn);
    }

    function leftBlinkChanged(value) {
        print("QQQQ left blink: " + value);

        if (value > 0.5) {
            if (!leftClosed) {
                leftClosed = true;
                if (rightClosed) {
                    go();
                }
            }
        } else {
            leftClosed = false;
        }
    }

    function rightBlinkChanged(value) {
        print("QQQQ right blink: " + value);

        if (value > 0.5) {
            if (!rightClosed) {
                rightClosed = true;
                if (leftClosed) {
                    go();
                }
            }
        } else {
            rightClosed = false;
        }
    }

    function cleanUp() {
        inputMapping.disable();
    }

    Script.scriptEnding.connect(function () {
        cleanUp();
    });

    inputMapping.from(Controller.Standard.LeftEyeBlink).peek().to(leftBlinkChanged);
    inputMapping.from(Controller.Standard.RightEyeBlink).peek().to(rightBlinkChanged);
    Controller.enableMapping(mappingName);
})();
