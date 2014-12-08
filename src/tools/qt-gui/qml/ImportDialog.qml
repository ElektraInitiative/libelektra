import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Dialogs 1.1
import "MainFunctions.js" as MFunctions

BasicWindow {

	title: qsTr("Import Configuration from File")
	height: Math.ceil(importMergeGroup.height*4)

	property alias importTextField: importTextField

	BasicRectangle {
		anchors.fill: parent

		ColumnLayout {

			anchors.fill: parent
			anchors.margins: defaultMargins
			spacing: defaultSpacing

			Label {
				text: qsTr("Please select a file to import to \"" + path.text + "\": ")
			}
			RowLayout {
				TextField {
					id: importTextField
					Layout.fillWidth: true
				}
				Button {
					text: "..."
					implicitWidth: importTextField.height
					onClicked: {
						importFileDialog.nameFilters = guiBackend.nameFilters()
						importFileDialog.open()
					}
				}
			}
			GroupBox {
				id: importMergeGroup

				title: qsTr("Merge Strategy:")
				flat: true

				RowLayout {
					ExclusiveGroup { id: group }
					Column {
						RadioButton {
							id: preserve
							text: qsTr("Preserve")
							exclusiveGroup: group
							checked: true
							property string command: "preserve"
						}
						RadioButton {
							text: qsTr("Ours")
							exclusiveGroup: group
							property string command: "ours"
						}
					}
					Column {
						RadioButton {
							text: qsTr("Theirs")
							exclusiveGroup: group
							property string command: "theirs"
						}
						RadioButton {
							text: qsTr("Base")
							exclusiveGroup: group
							property string command: "base"
						}
					}
					Column {
						RadioButton {
							text: qsTr("New Key")
							exclusiveGroup: group
							property string command: "newkey"
						}
						RadioButton {
							text: qsTr("Our Value")
							exclusiveGroup: group
							property string command: "ourvalue"
						}
					}
					Column {
						RadioButton {
							text: qsTr("Their Value")
							exclusiveGroup: group
							property string command: "theirvalue"
						}
						Item{
							height: preserve.height
							width: preserve.width
						}
					}
				}
			}
		}
	}

	cancelButton.onClicked: {
		importTextField.text = ""
		importDialog.close()
	}

	okButton.onClicked: {
		if(importTextField.text !== ""){
			var plugin = importFileDialog.selectedNameFilter.match(/[A-Z]+/).toString()
			plugin = plugin.toLowerCase()

			undoManager.createImportConfigurationCommand(externTreeModel, treeView.currentNode.path, plugin, importTextField.text, group.current.command)
			externTreeModel.refresh()
			importTextField.text = ""
			preserve.checked = true
			importDialog.close()
		}
		else{
			MFunctions.showMessage(qsTr("No Input"), qsTr("Please enter the path of a compatible configuration file."), "", "", "w")
		}
	}
}
