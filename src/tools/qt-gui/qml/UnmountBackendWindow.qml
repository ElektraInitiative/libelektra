import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

    title: qsTr("Unmount Backend")

    contents: ColumnLayout {
        anchors.fill: parent
        anchors.margins: defaultMargins
        anchors.centerIn: parent
        spacing: defaultMargins

        Label {
            text: qsTr("Mounted Backends")
        }
        BasicRectangle {
            id: mountedBackendsFrame
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.alignment: Qt.AlignHCenter

            ScrollView {
                anchors.fill: parent
                anchors.margins: defaultMargins

                ListView {
                    id: mountedBackendsView
                    anchors.fill: parent
                    model: mountedBackendsModel
                    focus: true
                    //interactive: true
                    currentIndex: 0
                    highlight: Rectangle {
                        color: activePalette.highlight
                        width: mountedBackendsFrame.width
                    }
                    delegate: Text {
                        color: activePalette.text
                        text: backendName

                        MouseArea {
                            anchors.fill: parent
                            onClicked: mountedBackendsView.currentIndex = index
                        }
                    }
                }
            }
        }
        Button {
            text: qsTr("Unmount")
            Layout.alignment: Qt.AlignHCenter
        }
    }
}
