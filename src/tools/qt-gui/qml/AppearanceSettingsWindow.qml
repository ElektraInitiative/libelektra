import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

	title: qsTr("Choose Colors")

	property bool colorEdited: false

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
				height: resetButton.height
				Layout.fillWidth: true
			}
			Rectangle {
				height: resetButton.height
				width: resetButton.width
				border.color: "black"
				color: guiSettings.highlightColor
				MouseArea {
					anchors.fill: parent
					onClicked: {
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
				color: guiSettings.frameColor
				MouseArea {
					anchors.fill: parent
					onClicked: {
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
				color: guiSettings.nodeWithKeyColor
				MouseArea {
					anchors.fill: parent
					onClicked: {
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
				color: guiSettings.nodeWithoutKeyColor
				MouseArea {
					anchors.fill: parent
					onClicked: {
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
					guiSettings.highlightColor = activePalette.highlight
					guiSettings.frameColor =  activePalette.dark
					guiSettings.nodeWithKeyColor = activePalette.windowText
					guiSettings.nodeWithoutKeyColor = disabledPalette.windowText
					guiSettings.reset()
				}
			}
		}
	}
	cancelButton.action.text: qsTr("&Close")
	cancelButton.action.onTriggered: {
		if (colorEdited)
			guiSettings.setKDB()
		close()
		colorEdited = false
	}
	okButton.visible: false
}

