import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

	title: qsTr("Choose Appearance")

	property bool optionEdited: false

	ColumnLayout {
		Layout.fillHeight: false
		anchors.fill: parent
		anchors.margins: defaultMargins
		spacing: defaultSpacing
		GroupBox {
			id: colorSettings

                        Layout.alignment: Qt.alignTop | Qt.alignLeft
			Layout.leftMargin: (parent.parent.width - width) / 2 - width / 16
			Layout.topMargin: parent.parent.height / 32
			Layout.bottomMargin: width / 10
			title: qsTr("Choose Colors")
			GridLayout {
				anchors.fill: parent
				anchors.margins: defaultMargins
				columns: 2
				rowSpacing: parent.parent.parent.height / 16
				columnSpacing: parent.parent.parent.width / 16
				Label {
					text: qsTr("Highlight Color")
				}
				Rectangle {
					height: resetButton.height
					width: themeSwitch.width
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
					width: longestText.width
					text: qsTr("Frame Color")
				}
				Rectangle {
					height: resetButton.height
					width: themeSwitch.width
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
					width: longestText.width
					text: qsTr("Node with Key Color")
				}
				Rectangle {
					height: resetButton.height
					width: themeSwitch.width
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
					id: longestText
					text: qsTr("Node without Key Color")
				}
				Rectangle {
					height: resetButton.height
					width: themeSwitch.width
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
					text: qsTr("Reset to system colors")
					Layout.alignment: Qt.AlignRight
					width: parent.width
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
		GroupBox{
			flat: false
			title: qsTr("Icon Theme")
                        Layout.alignment: Qt.AlignLeft
			Layout.leftMargin: (parent.parent.width - width) / 2 - width / 16
			Layout.topMargin: parent.parent.height / 32
			Layout.bottomMargin: width / 10
			GridLayout {
				anchors.fill: parent
				anchors.margins: defaultMargins
				columns: 2
				rowSpacing: parent.parent.parent.height / 16
				columnSpacing: parent.parent.parent.width / 16
				Label {
					width: longestText.width
					text: qsTr("Use system icon theme")
				}
				Switch {
					id: themeSwitch
					checked: guiSettings.useSystemIconTheme
					onCheckedChanged: {
						guiSettings.useSystemIconTheme = checked
						optionEdited = true
					}
				}
			}
		}
	}
	cancelButton.action.text: qsTr("&Close")
	cancelButton.action.onTriggered: {
		close()
	}
	onClosing: {
		if (optionEdited) {
			guiSettings.setKDB()
			treeView.treeModel.synchronize()
			treeView.treeModel.refresh()
			optionEdited = false
		}
	}
	okButton.visible: false
}

