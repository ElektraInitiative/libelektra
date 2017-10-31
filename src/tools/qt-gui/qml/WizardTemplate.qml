import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

Item {
	id: template

	property alias	wizardText: wizardText
	property alias	buttonRow: buttonRow
	property alias	label: label
	property alias	textField: textField
	property bool	buttonVisible: false
	property alias	fileDialog: fileDialog
	Component.onCompleted: textField.forceActiveFocus()

	Connections {
		target: wizardLoader
		onClosing: {
			textField.text = ""
			loader.source = "Page1.qml"
		}
	}

	BasicRectangle {
		id: wizardRectangle

		anchors {
			top: parent.top
			left: parent.left
			right: parent.right
			bottom: buttonRow.top
			margins: defaultMargins
		}

		Text {
			id: wizardText

			anchors.right: parent.right
			anchors.left: parent.left
			anchors.top: parent.top
			anchors.bottom: wizardRow.top
			anchors.margins: defaultMargins
			verticalAlignment: Text.AlignVCenter
			wrapMode: Text.WordWrap
			color: activePalette.text
		}

		RowLayout {
			id: wizardRow

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom
			anchors.margins: defaultMargins
			spacing: defaultSpacing

			Label {
				id:label
			}
			TextField {
				id:textField

				Layout.fillWidth: true

				Keys.onPressed: {
					if (event.key === Qt.Key_Enter || event.key === Qt.Key_Return){
						buttonRow.nextButton.action.trigger()
						event.accepted = true
					}
					else if (event.key === Qt.Key_Escape)
						buttonRow.cancelButton.action.trigger()
				}
			}
			Button {
				id: fileDialogButton

				implicitWidth: textField.height
				implicitHeight: textField.height
				text: "..."
				visible: buttonVisible
				onClicked: {
					fileDialog.nameFilters = ["All Files (*.*)"]
					fileDialog.open()
				}
			}
		}
	}
	ButtonRow {
		id: buttonRow

		cancelButton.action.onTriggered: {
			wizardLoader.close()
			textField.text = ""
			loader.source = "Page1.qml"
		}
	}


	FileDialog {
		id: fileDialog

		title: qsTr("Select File")
	}
}
