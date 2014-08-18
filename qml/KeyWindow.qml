import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {
    id: keyWindow

//    property alias  valueLayout: valueLayout
    property alias  nameLabel: nameLabel
    property alias  addButton: addButton
    property alias  metaKeyModel: qmlMetaKeyModel
    property alias  nameTextField: nameTextField
    property alias  valueTextField: valueTextField
    property string path: ""
    property string keyName: ""
    property string keyValue: ""
    property bool   isEdited: false

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
        GridLayout {
            columns: 2
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
        //        RowLayout {
        //            spacing: defaultSpacing

        //            Label {
        //                id:nameLabel
        //                text: qsTr("Key Name: ")
        //            }
        //            TextField {
        //                id: nameTextField
        //                Layout.fillWidth: true
        //                focus: true
        //                text: keyName
        //            }
        //        }
        //        RowLayout {
        //            id:valueLayout
        //            spacing: defaultSpacing

        //            Label {
        //                id: valueLabel
        //                text: qsTr("Key Value:  ")
        //            }
        //            TextField {
        //                id: valueTextField
        //                Layout.fillWidth: true
        //                text: keyValue
        //            }
        //        }
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
                    model: qmlMetaKeyModel
                    delegate: metaKeyDelegate
                }
            }

            ListModel {
                id: qmlMetaKeyModel
            }
            Component {
                id: metaKeyDelegate

                NewMetaKey {
                    metaNameField.onTextChanged: qmlMetaKeyModel.set(index, {"metaName": metaNameField.text})
                    metaValueField.onTextChanged: qmlMetaKeyModel.set(index, {"metaValue": metaValueField.text})
                }
            }
        }
        Button {
            id: addButton
            anchors.horizontalCenter: parent.horizontalCenter
            text: qsTr("New Meta Key")
            onClicked: {
                //add visual item
                qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
            }
        }
    }
    cancelButton.onClicked: {
        keyWindow.visible = false
        nameTextField.undo()
        valueTextField.undo()
        qmlMetaKeyModel.clear()
    }
    okButton.onClicked: {
        //check if user has edited keyname or keyvalue
        if(keyName !== nameTextField.text || keyValue !== valueTextField.text)
            isEdited = true

        //TODO: check if user has edited the metakeys
        console.log("namefield " + qmlMetaKeyModel.get(0).metaName)
//        if(qmlMetaKeyModel.get(0).metaName !== qmlMetaKeyModel.get(0).metaNameField)
//            isEdited = true

        keyWindow.visible = false
        editAccepted()
    }
}
