import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Dialogs 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1

BasicWindow {
	id: generalMessageDialog

	property string text
	property string detailedText
	property alias	icon: icon

	cancelButton.visible: false
	detailsButton.visible: detailedText.length > 0
	detailsButton.action.onTriggered: detailsRectangle.state === "" ? detailsRectangle.state = "SHOW_DETAILED_TEXT" : detailsRectangle.state = ""

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
				source: "icons/dialog-error.png"
			}

			Text {
				id: mainText

				anchors.left: icon.right
				anchors.leftMargin: defaultMargins
				anchors.verticalCenter: icon.verticalCenter
				width: parent.width - icon.width - defaultMargins
				text: generalMessageDialog.text
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
				id: detailedText

				anchors.fill: parent
				frameVisible: false
				backgroundVisible: false
				text: generalMessageDialog.detailedText
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
						target: generalMessageDialog
						height: mainTextItem.implicitHeight + detailedText.contentItem.contentHeight + okButton.implicitHeight + 6*defaultMargins
					}
					PropertyChanges {
						target: detailsButton
						action.text: qsTr("&Hide Details")
					}
				}
			]
		}
	}

	okButton.action.onTriggered: {
		generalMessageDialog.close()
		error = false
	}
}
