import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.0
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

BasicWindow {

	title: qsTr("Unmount Backend")

	contents: ColumnLayout {
		anchors.fill: parent

		Text{
			text: qsTr("Notice: To successfully unmount backends you need to be an administrator. This action cannot be undone.")
			wrapMode: Text.WordWrap
			color: "#640000"
		}

		Label {
			text: qsTr("Mounted Backends")
		}
		BasicRectangle {
			id: mountedBackendsFrame
			Layout.fillWidth: true
			Layout.fillHeight: true
			Layout.alignment: Qt.AlignHCenter

			ScrollView {
				anchors.fill: parent
				anchors.margins: defaultMargins

				ListView {
					id: mountedBackendsView

					anchors.fill: parent
					model: externTreeModel.mountedBackends()
					focus: true
					interactive: true
					currentIndex: -1
					highlightMoveDuration: 0
					highlightResizeDuration: 0
					keyNavigationWraps: true

					highlight: Rectangle {
						color: activePalette.highlight
						width: mountedBackendsFrame.width
					}
					delegate: Text {
						color: modelData === "empty" ? disabledPalette.text : activePalette.text
						text:  modelData === "empty" ? qsTr("There are currently no mounted backends.") : modelData

						MouseArea {
							anchors.fill: parent
							onClicked: mountedBackendsView.currentIndex = index
						}
					}
				}
			}
		}
		Button {
			text: qsTr("Unmount")
			Layout.alignment: Qt.AlignHCenter
			onClicked: {
				if(mountedBackendsView.model.toString() !== "empty"){
					externTreeModel.unMountBackend(mountedBackendsView.currentItem.text)
					mountedBackendsView.model = externTreeModel.mountedBackends()
					externTreeModel.synchronize()

					if(mountedBackendsView.model.toString() === "empty")
						mountedBackendsView.currentIndex = -1

				}

				externTreeModel.refresh()
				metaAreaModel = null
				keyAreaModel = null
			}

		}

	}
	cancelButton.visible: false
	okButton.text: qsTr("Close")
}
