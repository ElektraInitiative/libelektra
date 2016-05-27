import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1
import QtQuick.Controls.Styles 1.1
import org.libelektra.qtgui 1.0
import "MainFunctions.js" as MFunctions

Item {
	id: page2

	property bool	includeStorage: true
	property bool	includeResolver: true
	property var	config: []
	property var	configModel: ListModel {
		id: configModel
	}
	property bool	contextMenuEnabled: pluginConfigTreeView.currentItem !== null
	property int	stackIndex: undoManager.index()
	property alias	includedPluginsModel: includedPluginsModel

	Connections {
		target: wizardLoader
		onClosing: {
			includedPluginsModel.clear()
			loader.source = "Page1.qml"
			includeStorage = true
			includeResolver = true
		}
	}

	Keys.onPressed: {
		if (event.key === Qt.Key_Enter || event.key === Qt.Key_Return){
			if (buttonRow.nextButton.action.enabled){
				buttonRow.nextButton.action.trigger()
				event.accepted = true
			}
		}
		else if (event.key === Qt.Key_Escape)
			buttonRow.cancelButton.action.trigger()
	}

	ColumnLayout {

		anchors {
			left: parent.left
			right: parent.right
			bottom: buttonRow.top
			top: parent.top
			margins: defaultMargins
		}

		RowLayout {
			spacing: defaultSpacing

			Text {
				id: text

				Layout.fillWidth: true
				wrapMode: Text.WordWrap
				color: activePalette.text
				text: qsTr("Please select the plugins you want to include in the backend. Make sure to add exactly one resolver, " +
						   "one storage plugin and other plugins they might need.")
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
				iconName: (guiSettings.useSystemIconTheme) ? "list-add" : ""
				tooltip: qsTr("Add Plugin")

				onClicked: {
					if (!alreadyInList(pluginDropdown.currentText)){
						guiBackend.addPlugin(pluginDropdown.currentText)

						if (!error){
							includedPluginsModel.append({"pluginName" : pluginDropdown.currentText})
							buttonRow.nextButton.action.enabled = guiBackend.validated()

							if (pluginDropdown.currentText.indexOf("[storage]") > -1)
								includeStorage = false
							else if (pluginDropdown.currentText.indexOf("[resolver]") > -1)
								includeResolver = false

							clearConfig()
							configModel.clear()
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
					id: includedPluginsView

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
			anchors.right: configInfoSwitch.left
			anchors.bottom: parent.bottom
			anchors.topMargin: defaultSpacing
			anchors.leftMargin: defaultMargins
			anchors.rightMargin: defaultSpacing
			implicitWidth: Math.ceil(wizardLoader.width*0.7)

			TreeView {
				id: pluginConfigTreeView

				treeModel: guiBackend.pluginConfigModel()
				visible: false
				toolTipParent: page2

				function expand(model, itemLoader){
					itemLoader.expanded = !itemLoader.expanded
					model.isExpanded = itemLoader.expanded
				}

				function mousePressed(mouse, model, itemLoader) {
					if (mouse.button === Qt.LeftButton){
						currentNode = model
						currentItem = itemLoader
						forceActiveFocus()
					}
					else if (mouse.button === Qt.RightButton){
						p2cmNew.action.enabled = contextMenuEnabled
						page2ContextMenu.popup()
					}
				}

				function getOpacity(model) {
					if (model.childCount > 0)
						return 1
					return 0
				}

				function getExpanded(model) {
					return model.isExpanded
				}
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

		ToolButton {
			id: configInfoSwitch

			anchors.right: parent.right
			anchors.top: pluginInfoRectangle.top

			iconSource:  "icons/applications-system"
			iconName:  (guiSettings.useSystemIconTheme) ? "applications-system" : ""
			tooltip: qsTr("Edit Plugin Configuration")

			onClicked: {
				if (page2.state === ""){
					pluginConfigTreeView.treeModel.refresh()
					page2.state = "SHOW_CONFIG_VIEWER"
				}
				else
					page2.state = ""
			}
		}
	}

	ButtonRow {
		id: buttonRow

		Component.onCompleted: nextButton.action.enabled = false
		nextButton.action.onTriggered: {
			loader.source = "Page3.qml"
			includeStorage = true
			includeResolver = true
			includedPluginsModel.clear()

			if (undoManager.index() > stackIndex)
				undoManager.setIndex(stackIndex)
		}
		cancelButton.action.onTriggered: {
			wizardLoader.close()
			includedPluginsModel.clear()
			loader.source = "Page1.qml"
			includeStorage = true
			includeResolver = true
		}
	}

	states:
		State {
		name: "SHOW_CONFIG_VIEWER"

		PropertyChanges {
			target: pluginConfigTreeView
			visible: true
		}
		PropertyChanges {
			target: infoText
			visible: false
		}
		PropertyChanges {
			target: pluginInfoLabel
			text: qsTr("Add Key to Configuration for Plugin %1").arg(pluginDropdown.currentText.replace(/\[\w*\]/,""))
		}
		PropertyChanges {
			target: configInfoSwitch
			iconSource: "icons/help-about.png"
			iconName: (guiSettings.useSystemIconTheme) ? "help-about" : ""
			tooltip: qsTr("Show Plugin Info")
		}
	}

	function alreadyInList(plugin) {

		for (var i = 0; i < includedPluginsModel.count; i++){
			if (includedPluginsModel.get(i).pluginName === plugin)
				return true
		}
		return false
	}

	function alreadyInConfig(key) {
		for (var i = 0; i < config.length; i++){
			if (config[i].toString() === key)
				return true
			return false
		}
	}

	function clearConfig() {

		while (config.length > 0) {
			config.pop();
		}
	}

	Menu {
		id: page2ContextMenu

		MenuItem {
			id: p2cmNew
			action: Action {
				text: qsTr("New Key ...")
				iconSource: "icons/document-new.png"
				iconName: (guiSettings.useSystemIconTheme) ? "document-new" : ""
				tooltip: qsTr("New Key")
				enabled: contextMenuEnabled
				onTriggered: newPluginConfigWindow.show()
			}
		}
		MenuItem {
			id: p2cmEdit
			action: Action {
				iconSource: "icons/edit-rename.png"
				iconName: (guiSettings.useSystemIconTheme) ? "edit-rename" : ""
				text: qsTr("Edit ...")
				tooltip: qsTr("Edit")
				enabled: contextMenuEnabled

				onTriggered: {
					editKeyWindow.selectedNode = pluginConfigTreeView.currentNode
					editKeyWindow.populateMetaArea()
					editKeyWindow.show()
				}
			}
		}
		MenuItem {
			id: p2cmDelete
			action: Action {
				text: qsTr("Delete")
				iconSource: "icons/document-close.png"
				iconName: (guiSettings.useSystemIconTheme) ? "document-close" : ""
				tooltip: qsTr("Delete")
				shortcut: StandardKey.Delete
				enabled: contextMenuEnabled
				onTriggered: MFunctions.deleteBranch(pluginConfigTreeView)
			}
		}
	}

	NewKeyWindow {
		id: newPluginConfigWindow

		title: qsTr("Create new configuration Key for plugin %1").arg(pluginDropdown.currentText.replace(/\[\w*\]/,""))
		path: pluginConfigTreeView.currentNode === null ? "" : pluginConfigTreeView.currentNode.path

		function editAccepted() {

			var metaData = {};

			//collect metadata
			for (var i = 0; i < qmlMetaKeyModel.count; i++)
				metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

			dataContainer.clearData()
			dataContainer.setNewName(nameTextField.text)
			dataContainer.setNewValue(valueTextField.text)
			dataContainer.setNewMetadata(metaData)

			//create UndoCommand
			undoManager.createNewKeyCommand(pluginConfigTreeView.currentNode.parentModel, pluginConfigTreeView.currentNode.index, dataContainer, false)

			if (nameTextField.text.lastIndexOf("/") > 0)
				pluginConfigTreeView.currentNode.parentModel.refresh()

			visible = false
			qmlMetaKeyModel.clear()
			nameTextField.text = ""
			valueTextField.text = ""
			pluginConfigTreeView.treeModel.refresh()
		}
	}

	DataContainer {
		id: dataContainer
	}
}

