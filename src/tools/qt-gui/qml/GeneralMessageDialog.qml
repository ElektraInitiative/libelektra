import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Dialogs 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1

BasicWindow {
	id: generalMessageDialog

	property string title
	property string text
	property string detailedText
	property alias	icon: icon

	cancelButton.visible: false
	detailsButton.visible: detailedText.length > 0
	detailsButton.onClicked: detailsRectangle.state === "" ? detailsRectangle.state = "SHOW_DETAILED_TEXT" : detailsRectangle.state = ""
	okButton.onClicked: {
		generalMessageDialog.close()
		error = false
	}
	height: mainTextItem.height + okButton.height +  3*defaultMargins
	width: mainWindow.width*0.25

	contents: ColumnLayout {
		id: contentColumn

		spacing: defaultMargins
		anchors.fill: parent

		Item {
			id: mainTextItem

			width: parent.width
			height: Math.max(icon.height, mainText.height + defaultMargins)

			Image {
				id: icon
				source: "icons/dialog-error.png"
			}

			Text {
				id: mainText

				anchors.left: icon.right
				anchors.leftMargin: defaultMargins
				anchors.verticalCenter: icon.verticalCenter
				width: contentColumn.width - icon.width - 2*defaultMargins
				text: generalMessageDialog.text
				color: activePalette.text
				font.weight: Font.Bold
				wrapMode: Text.WrapAnywhere
			}
		}
		BasicRectangle {
			id: detailsRectangle

			width: parent.width
			implicitHeight: detailedText.implicitHeight + defaultSpacing
			height: 0
			visible: false
			Layout.fillHeight: true
			Layout.fillWidth: true

			ScrollView {

				anchors.fill: parent

				TextEdit {
					id: detailedText

					text: generalMessageDialog.detailedText
					width: detailsRectangle.width
					wrapMode: Text.WordWrap
					color: activePalette.text
					textMargin: defaultMargins
					readOnly: true
				}
			}

			states: [
				State {
					name: "SHOW_DETAILED_TEXT"

					PropertyChanges {
						target: detailsRectangle
						height: Math.min(detailedText.height + 2*defaultMargins, Screen.desktopAvailableHeight*0.5)
						visible: true
					}
					PropertyChanges {
						target: generalMessageDialog
						height: mainTextItem.height + detailedText.height + okButton.height
					}
					PropertyChanges {
						target: detailsButton
						text: qsTr("Hide Details")
					}
				}
			]
		}
	}
}
