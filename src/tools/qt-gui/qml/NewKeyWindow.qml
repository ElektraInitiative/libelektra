import QtQuick 2.2
import "MainFunctions.js" as MFunctions

KeyWindow {

	title: qsTr("Create new Key")

	path: selectedNode === null ? "" : selectedNode.path

	function editAccepted() {

		var metaData = {};

		//collect metadata
		for(var i = 0; i < qmlMetaKeyModel.count; i++)
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

		//create UndoCommand
		undoManager.createNewKeyCommand(selectedNode.parentModel, selectedNode.index, nameTextField.text, valueTextField.text, metaData)

		if(!error){
			visible = false
			selectedNode = selectedNode.parentModel.get(selectedNode.index)

			if(selectedNode.childCount === 1){
				treeView.treeModel.refresh()
				MFunctions.resetKeyAreaModel()
			}

			if(nameTextField.text.lastIndexOf("/") > 0)
				selectedNode.parentModel.refresh()

			qmlMetaKeyModel.clear()
			nameTextField.text = ""
			valueTextField.text = ""
		}
	}
}
