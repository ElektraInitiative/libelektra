import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

Window {
	id: basicWindow

	property alias okButton: okButton
	property alias cancelButton: cancelButton
	property alias detailsButton: detailsButton
	default property alias contents: placeholder.children

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
			anchors.bottom: parent.bottom

			Button {
				id: detailsButton
				anchors.left: parent.left
				text: qsTr("Show Details")
				visible: false
			}
			Item {
				id: filler
				height: okButton.height
				Layout.fillWidth: true
			}
			Button {
				id:okButton
				text: "Ok"
				isDefault: true
			}
			Button {
				id:cancelButton
				text: "Cancel"
			}
		}
	}
}
