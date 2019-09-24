
/* global Script, Controller */
/* jshint loopfunc:true */

(function() {

    var mappingName = 'redirected-walking-test-' + Math.random();
    var inputMapping = Controller.newMapping(mappingName);

    var leftClosed = false;
    var rightClosed = false;

    var leftTurn = Quat.fromPitchYawRollDegrees(0, 3.0, 0);
    var closedValue = 0.7;

    function go() {
        print("blink");
        MyAvatar.orientation = Quat.multiply(MyAvatar.orientation, leftTurn);
    }

    function leftBlinkChanged(value) {
        if (value > closedValue) {
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
        if (value > closedValue) {
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
