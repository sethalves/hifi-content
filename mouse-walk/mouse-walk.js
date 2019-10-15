"use strict";

/* global Script, Overlays, Controller, Reticle */

(function() { // BEGIN LOCAL_SCOPE

    var windowDimensions = Controller.getViewportDimensions();

    var buttonWidth = windowDimensions.x / 15.0;
    var buttonHeight = buttonWidth;

    var buttonPositionX = buttonWidth * 1.5;
    var buttonPositionY = windowDimensions.y - buttonHeight * 2.5;

    var deadSpaceSize = buttonWidth / 2.0;

    var mappingName = 'mouse-walk-' + Math.random();
    var inputMapping = Controller.newMapping(mappingName);

    var walkingForward = 0.0;
    var turning = 0.0;

    var buttonID = Overlays.addOverlay("image", {
        x: buttonPositionX,
        y: buttonPositionY,
        width: buttonWidth,
        height: buttonHeight,
        // subImage: {
        //     x: 0,
        //     y: 0,
        //     width: buttonWidth,
        //     height: buttonHeight
        // },
        imageURL: Script.resolvePath("circle.svg"),
        visible: true,
        alpha: 1.0
    });

    var walking = false;
    var clickDownPosition = null;
    var leftDeadZone = false;
    var jumping = 0;

    function moveEvent(event) {
        if (walking) {
            var dx = event.x - clickDownPosition.x;
            var dy = event.y - clickDownPosition.y;
            if (Math.abs(dx) > Math.abs(dy)) {
                walkingForward = 0.0;
                if (dx < -deadSpaceSize || dx > deadSpaceSize) {
                    turning = dx / buttonWidth;
                    leftDeadZone = true;
                } else {
                    turning = 0.0;
                }
            } else {
                turning = 0.0;
                if (dy < -deadSpaceSize || dy > deadSpaceSize) {
                    walkingForward = dy / buttonHeight;
                    leftDeadZone = true;
                } else {
                    walkingForward = 0.0;
                }
            }
        }
    }

    function pressEvent(event) {
        if (Overlays.getOverlayAtPoint(Reticle.position) == buttonID) {
            walking = true;
            leftDeadZone = false;
            clickDownPosition = {
                x: event.x,
                y: event.y
            };
            Controller.mouseMoveEvent.connect(moveEvent);
        }
    }

    function releaseEvent(event) {
        if (walking) {
            walkingForward = 0.0;
            turning = 0.0;
            walking = false;
            if (!leftDeadZone) {
                jumping = 20;
            }
            Controller.mouseMoveEvent.disconnect(moveEvent);
        }
    }

    Controller.mousePressEvent.connect(pressEvent);
    Controller.mouseReleaseEvent.connect(releaseEvent);

    inputMapping.from(function() {
        return walkingForward;
    }).to(Controller.Actions.TranslateZ);

    inputMapping.from(function() {
        return turning;
    }).to(Controller.Actions.Yaw);

    inputMapping.from(function() {
        if (jumping > 0) {
            jumping -= 1;
            return 1.0;
        }
        return 0.0;
    }).to(Controller.Actions.TranslateY);

    Controller.enableMapping(mappingName);

    Script.scriptEnding.connect(function () {
        Overlays.deleteOverlay(buttonID);
        inputMapping.disable();

        Controller.mousePressEvent.disconnect(pressEvent);
        Controller.mouseMoveEvent.disconnect(moveEvent);
        Controller.mouseReleaseEvent.disconnect(releaseEvent);
    });

}()); // END LOCAL_SCOPE
