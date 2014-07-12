import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

    property alias valueLayout: valueLayout
    property alias nameLabel: nameLabel
    property alias addButton: addButton
    property alias metaKeyModel: metaKeyModel
    property string pathinfo
    property string keyName
    property string keyValue

    contents: ColumnLayout {
        anchors.fill: parent
        anchors.margins: defaultMargins
        anchors.centerIn: parent
        spacing: defaultMargins

        Text{
            id: pathInfo
            text: pathinfo
            color: disabledPalette.text
        }
        RowLayout {
            spacing: defaultSpacing

            Label {
                id:nameLabel
                text: qsTr("Key Name: ")
            }
            TextField {
                id: nameTextField
                Layout.fillWidth: true
                focus: true
                placeholderText: keyName
            }
        }
        RowLayout {
            id:valueLayout
            spacing: defaultSpacing

            Label {
                id: valueLabel
                text: qsTr("Key Value: ")
            }
            TextField {
                id: valueTextField
                Layout.fillWidth: true
                placeholderText: keyValue
            }
        }
        BasicRectangle {
            id: metaArea
            Layout.fillWidth: true
            Layout.fillHeight: true

            ScrollView {
                anchors.fill: parent
                anchors.margins: defaultSpacing
                ListView {
                    anchors.fill: parent
                    spacing: defaultMargins
                    model: metaKeyModel
                    delegate: myDelegate
                }
            }
            ListModel {
                id: metaKeyModel

                ListElement {
                    element: "NewMetaKey.qml"
                }
            }
            Component {
                id: myDelegate
                Item {
                    Loader {
                        source: element
                    }
                }
            }
        }
        Button {
            id: addButton
            anchors.horizontalCenter: parent.horizontalCenter
            text: qsTr("New Meta Key")
        }
    }
}
