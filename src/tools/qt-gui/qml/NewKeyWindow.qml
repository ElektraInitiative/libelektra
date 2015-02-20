import QtQuick 2.2
import "MainFunctions.js" as MFunctions

KeyWindow {

	title: qsTr("Create new Key")

	path: selectedNode === null ? "" : selectedNode.path

	property bool isBelow: false

	function editAccepted() {

		var metaData = {};

		//collect metadata
		for(var i = 0; i < qmlMetaKeyModel.count; i++)
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

		//create UndoCommand
		undoManager.createNewKeyCommand(selectedNode.parentModel, selectedNode.index, nameTextField.text, valueTextField.text, metaData, isBelow)

		if(!error){
			visible = false
			console.log(undoManager.undoText)
			if(undoManager.undoText === "newBranch"){
				keyAreaView.selection.clear()
				treeView.treeModel.refresh()
				keyAreaSelectedItem = null
			}

			qmlMetaKeyModel.clear()
			nameTextField.text = ""
			valueTextField.text = ""
		}
	}
}
