import QtQuick 2.2
import "MainFunctions.js" as MFunctions

KeyWindow {

	title: qsTr("Create new Key")

	path: selectedNode === null ? "" : selectedNode.isNamespaceRoot ? selectedNode.path + "/" : selectedNode.path

	property bool isBelow: false

	function editAccepted() {

		var metaData = {};

		//collect metadata
		for (var i = 0; i < qmlMetaKeyModel.count; i++)
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

		container.clearData()
		container.setNewName(nameTextField.text)
		container.setNewValue(valueTextField.text)
		container.setNewMetadata(metaData)

		//create UndoCommand
		undoManager.createNewKeyCommand(selectedNode.parentModel, selectedNode.index, container, isBelow)

		if (!error){
			visible = false

			if (undoManager.undoText === "newBranch"){
				keyAreaView.selection.clear()
//				treeView.treeModel.refresh()
//				keyAreaSelectedItem = null
			}

			qmlMetaKeyModel.clear()
			nameTextField.text = ""
			valueTextField.text = ""
			isBelow = false
		}
	}
}
