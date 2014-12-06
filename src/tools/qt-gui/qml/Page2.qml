import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1
import QtQuick.Controls.Styles 1.1

Item {
	id: page2

	property bool	includeStorage: true
	property bool	includeResolver: true
	property var	config: []

	ColumnLayout {

		anchors.left: parent.left
		anchors.right: parent.right
		anchors.bottom: buttonRow.top
		anchors.top: parent.top
		anchors.margins: defaultMargins

		RowLayout {
			spacing: defaultSpacing

			Text {
				id: text

				Layout.fillWidth: true
				wrapMode: Text.WordWrap
				color: activePalette.text
				text: qsTr("Please select the plugins you want to include in the backend. Make sure to add exactly one resolver, one storage plugin and other plugins they might need.")
			}
			ComboBox {
				id: pluginDropdown

				Layout.fillWidth: true
				model: guiBackend.availablePlugins(includeStorage, includeResolver)
				onCurrentTextChanged: infoText.text = guiBackend.pluginInfo(pluginDropdown.currentText)
			}
			Button {
				id: addButton

				implicitWidth:  pluginDropdown.height
				implicitHeight: pluginDropdown.height

				iconSource: "icons/list-add.png"
				tooltip: qsTr("Add Plugin")

				onClicked: {
					if(!alreadyInList(pluginDropdown.currentText)){
						guiBackend.addPlugin(pluginDropdown.currentText, config)

						if(!error){
							includedPluginsModel.append({"pluginName" : pluginDropdown.currentText})
							buttonRow.nextButton.enabled = guiBackend.validated()

							if(pluginDropdown.currentText.indexOf("[storage]") > -1)
							  includeStorage = false
							if(pluginDropdown.currentText.indexOf("[resolver]") > -1)
							  includeResolver = false

							clearConfig()
							page2.state = ""
						}
					}
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
			width: Math.ceil(wizardLoader.width*0.3)

			ScrollView {
				anchors.fill: parent
				anchors.margins: defaultSpacing

				ListView {
					id: includedBackendsView

					model: ListModel {
						id: includedPluginsModel
					}
					delegate: Rectangle {
						width: parent.width
						height: pluginLabel.paintedHeight
						color: "transparent"
						Label {
							id: pluginLabel
							anchors.fill: parent
							anchors.leftMargin: defaultSpacing
							text: pluginName
						}
					}
				}
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
			anchors.right: addKeyToConfigButton.visible ? addKeyToConfigButton.left : configInfoSwitch.left
			anchors.bottom: parent.bottom
			anchors.topMargin: defaultSpacing
			anchors.leftMargin: defaultMargins
			anchors.rightMargin: defaultSpacing
			implicitWidth: Math.ceil(wizardLoader.width*0.7)

			ConfigSelector {
				id: selector

				visible: false
			}

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
		Button {
			id: addKeyToConfigButton

			implicitWidth:  configInfoSwitch.implicitWidth
			implicitHeight: implicitWidth

			anchors.top: pluginInfoRectangle.top
			anchors.right: configInfoSwitch.left

			anchors.rightMargin: defaultSpacing

			iconSource: "icons/list-add.png"
			tooltip: qsTr("Add Key to Configuration for Plugin " + pluginDropdown.currentText.replace(/\[\w*\]/,""))
			visible: false
			enabled: selector.currentNode === null ? false : !selector.currentNode.isNull

			onClicked: config.push(selector.currentNode.path)
		}
		Button {
			id: configInfoSwitch

			implicitWidth:  pluginDropdown.height
			implicitHeight: implicitWidth

			anchors.right: parent.right
			anchors.top: pluginInfoRectangle.top

			iconSource:  "icons/applications-system"
			tooltip: qsTr("Show Configuration Selector")

			onClicked: page2.state === "" ? page2.state = "SHOW_CONFIG_SELECTOR" : page2.state = ""
		}
	}
	ButtonRow {
		id: buttonRow

		finishButton.visible: false

		nextButton.enabled: guiBackend.validated()

		nextButton.onClicked: {
			loader.source = "Page3.qml"
			includeStorage = true
			includeResolver = true
			includedPluginsModel.clear()
		}
		cancelButton.onClicked: {
			wizardLoader.close()
			guiBackend.deleteBackend()
			includedPluginsModel.clear()
			loader.source = "Page1.qml"
			includeStorage = true
			includeResolver = true
		}
	}

	function alreadyInList(plugin) {

		for(var i = 0; i < includedPluginsModel.count; i++){
			if(includedPluginsModel.get(i).pluginName === plugin)
				return true
		}
		return false
	}

	function clearConfig() {

		while(config.length > 0) {
			config.pop();
		}
	}

	states:
		State {
		name: "SHOW_CONFIG_SELECTOR"

		PropertyChanges {
			target: selector
			visible: true
		}
		PropertyChanges {
			target: infoText
			visible: false
		}
		PropertyChanges {
			target: addKeyToConfigButton
			visible: true
		}
		PropertyChanges {
			target: pluginInfoLabel
			text: addKeyToConfigButton.tooltip

		}
		PropertyChanges {
			target: configInfoSwitch
			iconSource: "icons/help-about.png"
			tooltip: qsTr("Show Plugin Info")
		}
	}
}

