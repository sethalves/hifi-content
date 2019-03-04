
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
        id: rezButtonText
        text: "Rez Puppet"
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
        id: rezButtonMouseArea
        anchors.fill: rezButtonText
        onClicked: {
            emitSendToScript({'method' : 'rez'});
        }
    }

    Text {
        id: derezButtonText
        text: "Derez Puppet"
        anchors {
            left: parent.left
            right: parent.right
            top: rezButtonText.bottom
        }
        // anchors.centerIn: parent
        font.pixelSize: 24
        style: Text.Sunken
        color: "white"
        styleColor: "black"
    }

    MouseArea {
        id: derezButtonMouseArea
        anchors.fill: derezButtonText
        onClicked: {
            emitSendToScript({'method' : 'derez'});
        }
    }

}
