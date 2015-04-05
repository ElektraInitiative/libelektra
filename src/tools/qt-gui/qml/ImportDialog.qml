import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Dialogs 1.1
import "MainFunctions.js" as MFunctions

BasicWindow {

	title: qsTr("Import Configuration from File")
	height: Math.ceil(importMergeGroup.height + importTextField.height + okButton.implicitHeight + spacer.height + 6*defaultMargins)
	width: Math.ceil(importMergeGroup.width + 2*defaultMargins)

	property alias importTextField: importTextField

	Component.onCompleted: {
		importFileDialog.nameFilters = guiBackend.nameFilters()
		importFileDialog.selectNameFilter("dump (*.ecf)")
		importTextField.forceActiveFocus()
	}

	contents: ColumnLayout {

		anchors.fill: parent
		spacing: defaultSpacing

		Label {
			text: qsTr("Please select a file to import to \"%1\": ").arg(path.text)
			Layout.fillWidth: true
			wrapMode: Text.WrapAnywhere
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
					importFileDialog.open()
				}
			}
			FileDialog {
				id: importFileDialog

				title: qsTr("Select File")
				onAccepted: importDialog.importTextField.text = importFileDialog.fileUrl.toString().replace("file://", "")
			}
		}
		Item {
			id: spacer
			height: defaultMargins
			Layout.fillWidth: true
		}
		GroupBox {
			id: importMergeGroup

			title: qsTr("Merge Strategy:")
			flat: true
			Layout.fillWidth: true

			GridLayout {
				rows: 2
				columns: 4
				columnSpacing: defaultMargins

				Label {
					text: qsTr("First: ")
				}
				MergeStrategyComboBox {
					id: first
					currentIndex: 1
				}
				Label {
					text: qsTr("Third: ")
				}
				MergeStrategyComboBox {
					id: third
				}
				Label {
					text: qsTr("Second: ")
				}
				MergeStrategyComboBox {
					id: second
				}
				Label {
					text: qsTr("Fourth: ")
				}
				MergeStrategyComboBox {
					id: fourth
				}
			}
		}
	}

	cancelButton.action.onTriggered: {
		importTextField.text = ""
		importDialog.close()
	}

	okButton.action.enabled: importTextField.text !== ""
	okButton.action.onTriggered: {

		var plugin = importFileDialog.selectedNameFilter.match(/[a-z]+/).toString()

		container.clearData()
		container.setImportName(treeView.currentNode.path)
		container.setFormat(plugin)
		container.setFile(importTextField.text)

		var strategies = [first.currentText, second.currentText, third.currentText, fourth.currentText]

		container.setMergeStrategies(strategies)

		undoManager.createImportConfigurationCommand(treeView.currentNode.parentModel, treeView.currentNode.index, container)

//		treeView.treeModel.refresh()
		importTextField.text = ""
		first.currentIndex = 1
		importDialog.close()
	}
}
