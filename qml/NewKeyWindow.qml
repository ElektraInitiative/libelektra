import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {
    id: editWindow

    property alias valueLayout: valueLayout
    property alias nameLabel: nameLabel
    property alias addButton: addButton
    property alias metaKeyModel: metaKeyModel
    property string path: ""
    property string keyName: ""
    property string keyValue: ""
    property int metaCount: 0

    cancelButton.onClicked: {
        editWindow.visible = false
        metaKeyModel.clear()
    }
    okButton.onClicked: {
        editWindow.visible = false
        editAccepted()
    }

    contents: ColumnLayout {
        anchors.fill: parent
        anchors.margins: defaultMargins
        anchors.centerIn: parent
        spacing: defaultMargins

        Text{
            id: pathInfo
            text: path
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
                text: keyName
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
                text: keyValue
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
                    id: metaKeyListView
                    anchors.fill: parent
                    spacing: defaultMargins
                    model: metaKeyModel
                    delegate: metaKeyDelegate
                }
            }
            ListModel {
                id: metaKeyModel
            }
            Component {
                id: metaKeyDelegate
                NewMetaKey {
                    metaNameField.onTextChanged: metaKeyModel.set(index, {"metaName": metaNameField.text})
                    metaValueField.onTextChanged: metaKeyModel.set(index, {"metaValue": metaValueField.text})
                }
            }
        }
        Button {
            id: addButton
            anchors.horizontalCenter: parent.horizontalCenter
            text: qsTr("New Meta Key")
            onClicked: {metaKeyModel.append({"metaName" : "", "metaValue" : ""}); metaAreaListView.model.insertRow(metaCount); metaCount++;}
        }
    }

    function populateMetaArea() {
        for(var i = 0; i < metaCount; i++){
            metaKeyModel.append({"metaName" : metaAreaListView.model.get(i).name, "metaValue" : metaAreaListView.model.get(i).value})
        }
    }

    function editAccepted() {
        keyAreaView.model.setDataValue(keyAreaView.currentRow, nameTextField.text, "Name")
        keyAreaView.model.setDataValue(keyAreaView.currentRow, valueTextField.text, "Value")

        for(var i = 0; i < metaKeyModel.count; i++) {
            metaAreaListView.model.setDataValue(i, [metaKeyModel.get(i).metaName, metaKeyModel.get(i).metaValue], "MetaValue")
        }

        metaKeyModel.clear()
    }

}
