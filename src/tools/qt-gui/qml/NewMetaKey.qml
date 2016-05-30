import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.1
import "HelperFunctions.js" as Helper

Item {
	id: metaInfoItem

	width: parent.width
	height: metaNameField.height + defaultSpacing

	property alias metaNameField: metaNameField
	property alias metaValueField: metaValueField
	property alias deleteMetaButton: deleteMetaButton

	RowLayout {
		anchors.fill: parent
		anchors.margins: defaultMargins

		TextField {
			id: metaNameField
			Layout.fillWidth: isArray ? false : true
			placeholderText : qsTr("Meta Key Name ...")
			text: metaName
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
		TextField {
			id: metaValueField
			Layout.fillWidth: true
			text: metaValue
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
		ToolButton {
			id:deleteMetaButton

			implicitHeight: metaNameField.height + defaultSpacing
			implicitWidth: implicitHeight
			iconSource: "icons/edit-delete.png"
			iconName: Helper.userIcon(user-trash)

			onClicked: {
				qmlMetaKeyModel.remove(index)// remove the visual item

				if (isArray){
					for (var i = 0; i < qmlMetaKeyModel.count; i++){
						qmlMetaKeyModel.set(i, {"metaName": "#" + i})
					}

				}

				isEdited = true
			}

		}

	}

}


