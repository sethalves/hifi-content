import QtQuick 2.9
import QtQuick.Window 2.2
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
        // if (message.method === "enabled") { enabledToolButton.checked = message.value; }
        if (message.method === "clear") {
            console.log("QQQQ clear");
            scriptURLslistView.model.clear();
        }
        if (message.method === "whitelist") {
            message.value.forEach(function (url) {
                var data = {
                    name: url,
                    colorCode: "green"
                }
                urlsListModel.append(data);
            });
        }
        if (message.method === "greylist") {
            message.value.forEach(function (url) {
                var data = {
                    name: url,
                    colorCode: "grey"
                }
                urlsListModel.append(data);
            });
        }
        if (message.method === "blacklist") {
            message.value.forEach(function (url) {
                var data = {
                    name: url,
                    colorCode: "red"
                }
                urlsListModel.append(data);
            });
        }
        // if (message.method === "sync") { scriptURLslistView.model.sync(); }
    }


    ToolBar {
        id: toolBar
        anchors.right: parent.right
        anchors.rightMargin: 280
        anchors.left: parent.left
        anchors.leftMargin: 0
        anchors.top: parent.top
        anchors.topMargin: 0

        ToolButton {
            id: enabledToolButton
            text: qsTr("Enabled")
            checkable: true
        }
    }

    Connections {
        target: enabledToolButton
        onToggled: {
            emitSendToScript({ "method": "enabled", "value": enabledToolButton.checked });
        }
    }

    ListView {
        id: scriptURLslistView
        anchors.right: parent.right
        anchors.rightMargin: 0
        anchors.left: parent.left
        anchors.leftMargin: 0
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 0
        anchors.top: toolBar.bottom
        anchors.topMargin: 1
        model: ListModel {
            id: urlsListModel

            ListElement {
                name: "Grey"
                colorCode: "grey"
            }

            ListElement {
                name: "Red"
                colorCode: "red"
            }

            ListElement {
                name: "Blue"
                colorCode: "blue"
            }

            ListElement {
                name: "Green"
                colorCode: "green"
            }
        }
        delegate: Item {
            x: 5
            width: 80
            height: 40
            Row {
                id: row1
                spacing: 10
                Rectangle {
                    width: 40
                    height: 40
                    color: colorCode
                }

                Text {
                    text: name
                    font.bold: true
                    anchors.verticalCenter: parent.verticalCenter
                }
            }
        }
    }
}
