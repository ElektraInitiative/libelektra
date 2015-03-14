import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1

BasicWindow {

	title: qsTr("Plugin Info")
	okButton.visible: false
	cancelButton.action.text: qsTr("&Close")
	cancelButton.action.onTriggered: closeWindow()
	onClosing: pluginDropdown.currentIndex = 0
	spacer.height: 0

	contents: ColumnLayout {
		anchors.fill: parent
		spacing: defaultMargins

		RowLayout {
			Label {
				text: qsTr("Selected Plugin: ")
			}
			ComboBox {
				id: pluginDropdown

				Layout.fillWidth: true
				model: guiBackend.availablePlugins(true, true)
				onCurrentTextChanged: infoText.text = guiBackend.pluginInfo(pluginDropdown.currentText)
			}
		}
		BasicRectangle {
			Layout.fillWidth: true
			Layout.fillHeight: true

			TextArea {
				id: infoText

				anchors.fill: parent
				anchors.margins: defaultSpacing
				textFormat: Text.RichText
				backgroundVisible: false
				frameVisible: false
				readOnly: true
				wrapMode: Text.WordWrap
				onLinkActivated: {
					Qt.openUrlExternally(link)
				}
			}
		}
	}

	function closeWindow() {
		close()
		pluginDropdown.currentIndex = 0
	}
}

