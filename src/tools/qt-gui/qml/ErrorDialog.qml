import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Dialogs 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1

BasicWindow {
	id: dialog

	property string errorText
	property string detailedText
	property alias	icon: icon
	flags: Qt.Dialog

	height: mainTextItem.implicitHeight + okButton.implicitHeight +  3*defaultMargins
	width: mainWindow.width*0.25

	contents: ColumnLayout {
		id: contentColumn

		spacing: defaultMargins
		anchors.fill: parent

		Item {
			id: mainTextItem

			Layout.fillWidth: true
			height: Math.max(icon.implicitHeight, mainText.implicitHeight)

			Image {
				id: icon
				source: {
					if (title === "Error") (guiSettings.useSystemIconTheme) ? "image://theme/dialog-error" : "icons/dialog-error.png"
					else if (title === "Information")(guiSettings.useSystemIconTheme) ? "image://theme/dialog-information" : "icons/dialog-information.png"
					else if (title === "Warning") (guiSettings.useSystemIconTheme) ? "image://theme/dialog-warning" : "icons/dialog-warning.png"
					else (guiSettings.useSystemIconTheme) ? "image://theme/dialog-error" : "icons/dialog-error.png"
				}
			}

			Text {
				id: mainText

				anchors.left: icon.right
				anchors.leftMargin: defaultMargins
				anchors.verticalCenter: icon.verticalCenter
				width: parent.width - icon.width - defaultMargins
				text: errorText
				color: activePalette.text
				font.weight: Font.Bold
				wrapMode: Text.WrapAnywhere
			}
		}
		BasicRectangle {
			id: detailsRectangle

			visible: false
			Layout.fillHeight: true
			Layout.fillWidth: true

			TextArea {
				id: detailedTextArea

				anchors.fill: parent
				frameVisible: false
				backgroundVisible: false
				text: detailedText
				width: detailsRectangle.width
				wrapMode: Text.WordWrap
				textColor: activePalette.text
				textMargin: defaultMargins
				readOnly: true
			}

			states: [
				State {
					name: "SHOW_DETAILED_TEXT"

					PropertyChanges {
						target: detailsRectangle
						visible: true
					}
					PropertyChanges {
						target: dialog
						height: mainTextItem.implicitHeight + detailedTextArea.contentItem.contentHeight + okButton.implicitHeight + 6*defaultMargins
					}
					PropertyChanges {
						target: detailsButton
						action.text: qsTr("H&ide Details")
					}
				}
			]
		}
	}

	cancelButton.visible: false
	detailsButton.visible: detailedText.length > 0
	detailsButton.action.onTriggered: detailsRectangle.state === "" ? detailsRectangle.state = "SHOW_DETAILED_TEXT" : detailsRectangle.state = ""

	okButton.action.onTriggered: {
		visible = false
		destroy()
		error = false
	}
}
