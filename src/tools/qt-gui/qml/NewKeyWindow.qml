import QtQuick 2.2
import "MainFunctions.js" as MFunctions

KeyWindow {

	title: qsTr("Create new Key")
	path: treeView.currentNode === null ? "" : treeView.currentNode.path

	function editAccepted() {

		var metaData = {};

		//collect metadata
		for(var i = 0; i < qmlMetaKeyModel.count; i++)
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

		//create UndoCommand
		undoManager.createNewKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index, nameTextField.text, valueTextField.text, metaData)

		if(treeView.currentNode.childCount === 1)
			MFunctions.resetKeyAreaModel()

		if(nameTextField.text.lastIndexOf("/") > 0)
			treeView.currentNode.parentModel.refresh()

		qmlMetaKeyModel.clear()
		nameTextField.text = ""
		valueTextField.text = ""
	}
}
