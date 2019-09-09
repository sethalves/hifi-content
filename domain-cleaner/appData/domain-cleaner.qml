
import QtQuick 2.10
import QtQuick.Window 2.10
import QtQuick.Controls 2.3


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

    Button {
        id: saveButton
        x: 31
        y: 26
        text: qsTr("Save Domain")
    }

    Button {
        id: restoreButton
        x: 31
        y: 87
        text: qsTr("Restore Domain")
    }

    Connections {
        target: saveButton
        onClicked: {
            emitSendToScript({'method' : 'save'});
        }
    }

    Connections {
        target: restoreButton
        onClicked: {
            emitSendToScript({'method' : 'restore'});
        }
    }
}
