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
                    url: url,
                    colorCode: "green",
                    status: "white"
                }
                urlsListModel.append(data);
            });
        }
        if (message.method === "greylist") {
            message.value.forEach(function (url) {
                var data = {
                    url: url,
                    colorCode: "red",
                    status: "grey"
                }
                urlsListModel.append(data);
            });
        }
        if (message.method === "blacklist") {
            message.value.forEach(function (url) {
                var data = {
                    url: url,
                    colorCode: "red",
                    status: "black"
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
        }
        delegate: Item {
            x: 5
            // width: 80
            anchors.left: parent.left
            anchors.right: parent.right
            height: 20
            Row {
                id: urlListRow
                spacing: 10
                Rectangle {
                    width: 480
                    height: 20
                    color: colorCode

                    Text {
                        text: url
                        font.bold: false
                        anchors.verticalCenter: parent.verticalCenter
                        anchors.leftMargin: 5
                        anchors.left: parent.left
                    }

                    MouseArea {
                        id: urlListMouseArea
                        z: 1
                        hoverEnabled: false
                        anchors.fill: parent
                        onClicked: {
                            print("QQQQ HERE " + status + " " + url);
                            if (status === "white") {
                                status = "black";
                                colorCode = "red";
                            } else {
                                status = "white";
                                colorCode = "green";
                            }
                        }
                    }
                }
            }
        }
    }

    Connections {
        target: urlListRow
        onClicked: print("url clicked")
    }
}
