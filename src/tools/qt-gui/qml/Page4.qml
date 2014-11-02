import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.2

Item {
    id: page4

    ColumnLayout {

        anchors.fill: parent
        anchors.margins: defaultMargins

        RowLayout {
            spacing: defaultMargins

            Text {
                id: text
                Layout.fillWidth: true
                wrapMode: Text.WordWrap
                color: activePalette.text
                text: qsTr("Please select the plugins you want to include in the backend.")
            }
            ComboBox {
                id: pluginDropdown
                Layout.fillWidth: true
                model: [ "hexcode", "simpleini", "syslog" ]
            }
            RowLayout {
                spacing: defaultSpacing

                Button {
                    id: addButton
                    iconSource: "icons/list-add.png"
                    tooltip: qsTr("Add Plugin")
                }
                Button {
                    id: subButton
                    iconSource: "icons/list-remove.png"
                    tooltip: qsTr("Remove Plugin")
                }
            }
        }
        Item {
            Layout.fillWidth: true
            height: 18
        }

        RowLayout {
            spacing: defaultMargins

            ColumnLayout {
                spacing: defaultSpacing

                Label {
                    text: qsTr("Included Plugins")
                }
                BasicRectangle {
                    id: includedPluginsArea
                    width: Math.ceil(wizardLoader.width*0.3)
                    //Layout.fillWidth: true
                    Layout.fillHeight: true

                    ScrollView {

                    }
                }
            }
            ColumnLayout {
                spacing: defaultSpacing

                Label {
                    text: qsTr("Plugin Info")
                }
                BasicRectangle {
                    id: pluginInfoArea
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    ScrollView {
                        //Markdown
                    }
                }
            }
        }
        ButtonRow {
            backButton.onClicked: loader.source = "Page3.qml"
            finishButton.enabled: true
            nextButton.enabled: false
            cancelButton.onClicked: wizardLoader.visible = false
        }
    }
}
