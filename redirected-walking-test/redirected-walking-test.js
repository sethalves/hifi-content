
/* global Script, Controller, MyAvatar, Quat */
/* jshint loopfunc:true */

(function() {

    var mappingName = 'redirected-walking-test-' + Math.random();
    var inputMapping = Controller.newMapping(mappingName);

    var leftClosed = false;
    var rightClosed = false;

    var leftTurn = Quat.fromPitchYawRollDegrees(0, 3.0, 0);
    var closedValue = 0.7;

    var avatarOrientation = null;

    // avoid slow call into c++ Quat.multiply
    function QuatMultiply(a, b) {
        return {
            w: a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z,  // 1
            x: a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y,  // i
            y: a.w * b.y - a.x * b.z + a.y * b.w + a.z * b.x,  // j
            z: a.w * b.z + a.x * b.y - a.y * b.x + a.z * b.w   // k
        };
    }

    function go() {
        MyAvatar.orientation = QuatMultiply(avatarOrientation, leftTurn);
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
            avatarOrientation = MyAvatar.orientation;
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
            avatarOrientation = MyAvatar.orientation;
        }
    }

    // function leftBlinkChanged(value) {
    //     if (value > closedValue) {
    //         if (!leftClosed) {
    //             leftClosed = true;
    //             if (!rightClosed) {
    //                 go();
    //             }
    //         }
    //     } else {
    //         leftClosed = false;
    //         avatarOrientation = MyAvatar.orientation;
    //     }
    // }

    // function rightBlinkChanged(value) {
    //     if (value > closedValue) {
    //         if (!rightClosed) {
    //             rightClosed = true;
    //             if (!leftClosed) {
    //                 go();
    //             }
    //         }
    //     } else {
    //         rightClosed = false;
    //         avatarOrientation = MyAvatar.orientation;
    //     }
    // }


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
