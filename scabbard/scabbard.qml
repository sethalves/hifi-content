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
        if (message.method === "leftShoulderEnabled") { leftShoulderEnabledCB.checked = message.value; }
        if (message.method === "leftShoulderLocked") { leftShoulderLockedCB.checked = message.value; }
        if (message.method === "rightShoulderEnabled") { rightShoulderEnabledCB.checked = message.value; }
        if (message.method === "rightShoulderLocked") { rightShoulderLockedCB.checked = message.value; }
        if (message.method === "leftHipEnabled") { leftHipEnabledCB.checked = message.value; }
        if (message.method === "leftHipLocked") { leftHipLockedCB.checked = message.value; }
        if (message.method === "rightHipEnabled") { rightHipEnabledCB.checked = message.value; }
        if (message.method === "rightHipLocked") { rightHipLockedCB.checked = message.value; }
    }

    Text {
        id: text1
        x: 65
        y: 46
        text: qsTr("Left Shoulder")
        font.pixelSize: 15
    }

    CheckBox {
        id: leftShoulderEnabledCB
        x: 65
        y: 66
        width: 143
        height: 40
        text: qsTr("Enabled")
        font.family: "Arial"
        checkState: Qt.Checked
    }

    CheckBox {
        id: leftShoulderLockedCB
        x: 65
        y: 112
        width: 143
        height: 40
        text: qsTr("Locked")
        font.family: "Arial"
    }

    Text {
        id: text2
        x: 251
        y: 46
        text: qsTr("Right Shoulder")
        font.pixelSize: 15
    }

    CheckBox {
        id: rightShoulderEnabledCB
        x: 251
        y: 66
        width: 137
        height: 40
        text: qsTr("Enabled")
        font.family: "Arial"
        checkState: Qt.Checked
    }

    CheckBox {
        id: rightShoulderLockedCB
        x: 251
        y: 112
        width: 137
        height: 40
        text: qsTr("Locked")
        font.family: "Arial"
    }

    Text {
        id: text3
        x: 65
        y: 181
        text: qsTr("Left Hip")
        font.pixelSize: 15
    }

    Text {
        id: text4
        x: 251
        y: 181
        text: qsTr("Right Hip")
        font.pixelSize: 15
    }

    CheckBox {
        id: leftHipEnabledCB
        x: 65
        y: 201
        width: 143
        height: 40
        text: qsTr("Enabled")
        font.family: "Arial"
        checkState: Qt.Checked
    }

    CheckBox {
        id: leftHipLockedCB
        x: 65
        y: 247
        width: 143
        height: 40
        text: qsTr("Locked")
        font.family: "Arial"
    }

    CheckBox {
        id: rightHipEnabledCB
        x: 251
        y: 201
        width: 137
        height: 40
        text: qsTr("Enabled")
        font.family: "Arial"
        checkState: Qt.Checked
    }

    CheckBox {
        id: rightHipLockedCB
        x: 251
        y: 247
        width: 137
        height: 40
        text: qsTr("Locked")
        font.family: "Arial"
    }

    Connections {
        target: leftShoulderEnabledCB
        onToggled: {
            emitSendToScript({ "method": "leftShoulderEnabled", "value": leftShoulderEnabledCB.checked });
        }
    }

    Connections {
        target: leftShoulderLockedCB
        onToggled: {
            emitSendToScript({ "method": "leftShoulderLocked", "value": leftShoulderLockedCB.checked });
        }
    }

    Connections {
        target: rightShoulderEnabledCB
        onToggled: {
            emitSendToScript({ "method": "rightShoulderEnabled", "value": rightShoulderEnabledCB.checked });
        }
    }

    Connections {
        target: rightShoulderLockedCB
        onToggled: {
            emitSendToScript({ "method": "rightShoulderLocked", "value": rightShoulderLockedCB.checked });
        }
    }

    Connections {
        target: leftHipEnabledCB
        onToggled: {
            emitSendToScript({ "method": "leftHipEnabled", "value": leftHipEnabledCB.checked });
        }
    }

    Connections {
        target: leftHipLockedCB
        onToggled: {
            emitSendToScript({ "method": "leftHipLocked", "value": leftHipLockedCB.checked });
        }
    }

    Connections {
        target: rightHipEnabledCB
        onToggled: {
            emitSendToScript({ "method": "rightHipEnabled", "value": rightHipEnabledCB.checked });
        }
    }

    Connections {
        target: rightHipLockedCB
        onToggled: {
            emitSendToScript({ "method": "rightHipLocked", "value": rightHipLockedCB.checked });
        }
    }
}
