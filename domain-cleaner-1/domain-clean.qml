
import QtQuick 2.0

Rectangle {
    id: root
    width: 480
    height: 706

    signal sendToScript(var message);
    function emitSendToScript(message) {
        sendToScript(message);
    }

    function fromScript(message) {
        if (message.method === 'initialize') {
        }
    }

    Text {
        id: saveButtonText
        text: "Save Domain"
        anchors {
            left: parent.left
            right: parent.right
            top: parent.top
        }
        // anchors.centerIn: parent
        font.pixelSize: 24
        style: Text.Sunken
        color: "white"
        styleColor: "black"
    }

    MouseArea {
        id: saveButtonMouseArea
        anchors.fill: saveButtonText
        onClicked: {
            emitSendToScript({'method' : 'save'});
        }
    }

    Text {
        id: restoreButtonText
        text: "Restore Domain"
        anchors {
            left: parent.left
            right: parent.right
            top: saveButtonText.bottom
        }
        // anchors.centerIn: parent
        font.pixelSize: 24
        style: Text.Sunken
        color: "white"
        styleColor: "black"
    }

    MouseArea {
        id: restoreButtonMouseArea
        anchors.fill: restoreButtonText
        onClicked: {
            emitSendToScript({'method' : 'restore'});
        }
    }

}
