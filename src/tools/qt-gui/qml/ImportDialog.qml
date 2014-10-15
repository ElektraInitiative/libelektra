import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

    title: qsTr("Import Configuration from File")
    height: Math.round(importMergeGroup.height*4)

    property alias importTextField: importTextField

    BasicRectangle {
        anchors.fill: parent

        ColumnLayout {

            anchors.fill: parent
            anchors.margins: defaultMargins
            spacing: defaultSpacing

            Label {
                text: qsTr("File to import: ")
            }
            RowLayout {
                TextField {
                    id: importTextField
                    Layout.fillWidth: true
                }
                Button {
                    text: "..."
                    implicitWidth: importTextField.height
                    onClicked: importFileDialog.open()
                }
            }
            GroupBox {
                id: importMergeGroup

                title: qsTr("Merge Strategy:")
                flat: true

                RowLayout {
                    ExclusiveGroup { id: group }
                    RadioButton {
                        text: qsTr("Preserve")
                        exclusiveGroup: group
                        checked: true
                    }
                    RadioButton {
                        text: qsTr("Overwrite")
                        exclusiveGroup: group
                    }
                    RadioButton {
                        text: qsTr("Cut")
                        exclusiveGroup: group
                    }
                }
            }
        }
    }

    cancelButton.onClicked: {
        importTextField.text = ""
    }
    okButton.onClicked: {
        undoManager.createImportConfigurationCommand(externTreeModel, treeView.currentNode.path, "dump", importTextField.text, "preserve")
        importDialog.close()
    }
}
