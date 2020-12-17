import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1
import "MainFunctions.js" as MFunctions

BasicWindow {

	property alias  nameLabel: nameLabel
	property alias  addButton: addButton
	property alias  qmlMetaKeyModel: qmlMetaKeyModel
	property alias  nameTextField: nameTextField
	property alias  valueLabel: valueLabel
	property alias  valueTextField: valueTextField
	property bool   nameReadOnly: false
	property string valuePlaceHolder: "Meta Key Value ..."
	property int    modelIndex: 0
	property bool   isArray: false
	property string parentPath: selectedNode === null ? "" : selectedNode.isNamespaceRoot ? (selectedNode.path + "/") : (selectedNode.path.slice(0, selectedNode.path.lastIndexOf("/")) + (selectedNode.path.indexOf("/") === selectedNode.path.lastIndexOf("/") ? "/" : ""))
	property string path: !visible || selectedNode === null ? "" : parentPath
	property string keyName: ""
	property string keyValue: ""
	property bool   isEdited: false
	property var    selectedNode: null
	property bool   accessFromSearchResults: false

	onClosing: cancelClicked()

	contents: ColumnLayout {
		anchors.fill: parent
		spacing: defaultMargins

		Text{
			id: pathInfo
			text: path
			color: disabledPalette.text
			Layout.fillWidth: true
			wrapMode: Text.WrapAnywhere
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
				text: keyName
				clip: true
				Keys.onPressed: {
					if (event.key === Qt.Key_Enter || event.key === Qt.Key_Return){
						okClicked()
						event.accepted = true
					}
					else if (event.key === Qt.Key_Escape){
						cancelClicked()
						event.accepted = true
					}
				}
			}
			Label {
				id: valueLabel
				text: qsTr("Key Value: ")
			}
			TextField {
				id: valueTextField
				Layout.fillWidth: true
				text: keyValue
				clip: true
				Keys.onPressed: {
					if (event.key === Qt.Key_Enter || event.key === Qt.Key_Return){
						okClicked()
						event.accepted = true
					}
					else if (event.key === Qt.Key_Escape){
						cancelClicked()
						event.accepted = true
					}
				}
			}
		}
                Label {
                    id: metaKeyLabel
                    Layout.topMargin: defaultMargins
                    text: "Metakeys:"
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
					spacing: defaultSpacing
					model: qmlMetaKeyModel
					delegate: metaKeyDelegate
				}
			}

			ListModel {
				id: qmlMetaKeyModel

				onCountChanged: modelIndex = count
			}
			Component {
				id: metaKeyDelegate

				NewMetaKey {
					//check if user has edited metakeyname or metakeyvalue. This comparison can only happen here since
					//"metaNameField.text" cannot be accessed outside the delegate.
					metaNameField.readOnly: nameReadOnly
					metaValueField.placeholderText: valuePlaceHolder

					metaNameField.onTextChanged:  {
						if (metaName !== metaNameField.text){
							qmlMetaKeyModel.set(index, {"metaName": metaNameField.text})
							isEdited = true
						}
					}
					metaValueField.onTextChanged: {
						if (metaValue !== metaValueField.text){
							qmlMetaKeyModel.set(index, {"metaValue": metaValueField.text})
							isEdited = true
						}
					}
				}
			}
		}
		Button {
			id: addButton

                        Layout.alignment : Qt.AlignHCenter
			action: Action {
				text: qsTr("&New Meta Key")
				onTriggered: qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})  //add visual item
			}
		}
	}

	okButton.action.enabled:  nameTextField.text !== ""
	okButton.action.onTriggered: okClicked()
	cancelButton.action.onTriggered: cancelClicked()

	function okClicked(){
		//check if user has edited keyname or keyvalue
		if (keyName !== nameTextField.text || keyValue !== valueTextField.text)
			isEdited = true

		editAccepted()
	}

	function cancelClicked() {
		visible = false
		isEdited = false
		accessFromSearchResults = false
		qmlMetaKeyModel.clear()
//		selectedNode = null
		nameTextField.readOnly = false
		nameTextField.textColor = activePalette.text
	}
}
