import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1

Item {
	id: page4

	ColumnLayout {

		anchors.left: parent.left
		anchors.right: parent.right
		anchors.bottom: buttonRow.top
		anchors.top: parent.top
		anchors.margins: defaultMargins

		RowLayout {
			spacing: defaultMargins

			Text {
				id: text
				Layout.fillWidth: true
				wrapMode: Text.WordWrap
				color: activePalette.text
				text: qsTr("Please select the plugins you want to include in the backend.")
			}
			ComboBox {
				id: pluginDropdown
				Layout.fillWidth: true
				model: [ "hexcode", "simpleini", "syslog" ]
			}
			RowLayout {
				spacing: defaultSpacing

				Button {
					id: addButton

					iconSource: "icons/list-add.png"
					tooltip: qsTr("Add Plugin")
				}
				Button {
					id: subButton

					iconSource: "icons/list-remove.png"
					tooltip: qsTr("Remove Plugin")
				}
			}
		}
		Item {
			id: spacer

			Layout.fillWidth: true
			height: 2*defaultMargins
		}
		Label {
			id: includedPluginsLabel
			text: qsTr("Included Plugins")
			anchors.top: spacer.bottom
			anchors.left: parent.left
			anchors.bottomMargin: defaultSpacing
		}
		BasicRectangle {
			id: includedPluginsRectangle

			anchors.top: includedPluginsLabel.bottom
			anchors.left: parent.left
			anchors.right: pluginInfoRectangle.left
			anchors.bottom: parent.bottom
			anchors.topMargin: defaultSpacing
			anchors.rightMargin: defaultMargins
			width: Math.ceil(wizardLoader.width*0.4)

			ScrollView {

			}
		}
		Label {
			id: pluginInfoLabel
			text: qsTr("Plugin Info")
			anchors.top: spacer.bottom
			anchors.left: pluginInfoRectangle.left
		}
		BasicRectangle {
			id: pluginInfoRectangle

			anchors.top: pluginInfoLabel.bottom
			anchors.right: parent.right
			anchors.bottom: parent.bottom
			anchors.topMargin: defaultSpacing
			anchors.leftMargin: defaultMargins
			width: Math.ceil(wizardLoader.width*0.6)

			ScrollView {

			}
		}
	}
	ButtonRow {
		id: buttonRow

		backButton.onClicked: loader.source = "Page3.qml"
		finishButton.enabled: true
		nextButton.enabled: false
		cancelButton.onClicked: wizardLoader.visible = false
	}
}
