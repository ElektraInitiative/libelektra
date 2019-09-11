import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

Window {

	property alias okButton: okButton
	property alias cancelButton: cancelButton
	property alias detailsButton: detailsButton
	default property alias contents: placeholder.children
	property alias spacer: spacer

	width: Math.ceil(mainWindow.width*0.4)
	height: Math.ceil(mainWindow.height*0.6)

	modality: Qt.WindowModal

	x: Math.ceil(Screen.desktopAvailableWidth*0.5-width*0.5)
	y: Math.ceil(Screen.desktopAvailableHeight*0.5-height*0.5)

	color: activePalette.window

	ColumnLayout {
		anchors.fill: parent
		anchors.margins: defaultMargins
		spacing: defaultSpacing

		Item {
			id: placeholder

			Layout.fillHeight: true
			Layout.fillWidth: true
		}
		Item {
			id:spacer
			Layout.fillWidth: true
			height: 2*defaultMargins
		}
		RowLayout {
                        Layout.alignment : Qt.AlignBottom

			Button {
				id: detailsButton
                                Layout.alignment : Qt.AlignLeft
				visible: false
				action: Action {
					text: qsTr("&Show Details")
				}
			}
			Item {
				id: filler
				height: okButton.height
				Layout.fillWidth: true
			}
			Button {
				id:okButton
				action: Action {
					text: qsTr("&Ok")
				}
			}
			Button {
				id:cancelButton
				action: Action {
					text: qsTr("&Cancel")
				}
			}
		}
	}
}
