import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

	title: qsTr("Choose Colors")

	contents: BasicRectangle {
		anchors.fill: parent
		GridLayout {
			anchors.fill: parent
			anchors.margins: defaultMargins
			columns: 3
			rowSpacing: defaultSpacing

			Label {
				text: qsTr("Highlight Color")
			}
			Item {
				height: resetButton .height
				Layout.fillWidth: true
			}
			Rectangle {
				height: resetButton.height
				width: resetButton.width
				border.color: "black"
				color: settings.highlightColor
				MouseArea {
					anchors.fill: parent
					onDoubleClicked:{
						colorDialog.type = "highlight"
						colorDialog.open()
					}
				}
			}

			Label {
				text: qsTr("Frame Color")
			}
			Item {
				height: resetButton.height
				Layout.fillWidth: true
			}
			Rectangle {
				height: resetButton.height
				width: resetButton.width
				border.color: "black"
				color: settings.frameColor
				MouseArea {
					anchors.fill: parent
					onDoubleClicked:{
						colorDialog.type = "frame"
						colorDialog.open()
					}
				}
			}

			Label {
				text: qsTr("Node with Key Color")
			}
			Item {
				height: resetButton.height
				Layout.fillWidth: true
			}
			Rectangle {
				height: resetButton.height
				width: resetButton.width
				border.color: "black"
				color: settings.nodeWithKeyColor
				MouseArea {
					anchors.fill: parent
					onDoubleClicked:{
						colorDialog.type = "nodeWith"
						colorDialog.open()
					}
				}
			}

			Label {
				text: qsTr("Node without Key Color")
			}
			Item {
				height: resetButton.height
				Layout.fillWidth: true
			}
			Rectangle {
				height: resetButton.height
				width: resetButton.width
				border.color: "black"
				color: settings.nodeWithoutKeyColor
				MouseArea {
					anchors.fill: parent
					onDoubleClicked:{
						colorDialog.type = "nodeWithout"
						colorDialog.open()
					}
				}
			}

			Button {
				id: resetButton

				text: qsTr("Reset to defaults")
				Layout.rowSpan: 4
				anchors.right: parent.right

				onClicked: {
					settings.highlightColor = activePalette.highlight
					settings.frameColor =  activePalette.dark
					settings.nodeWithKeyColor = activePalette.windowText
					settings.nodeWithoutKeyColor = disabledPalette.windowText
				}
			}
		}
	}
	cancelButton.action.text: qsTr("&Close")
	cancelButton.action.onTriggered: {
		close()
	}
	okButton.visible: false
}

