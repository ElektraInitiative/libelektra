import QtQuick 2.2

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

		nameTextField.text = ""
		valueTextField.text = ""
		//		externTreeModel.refresh()
		if(treeView.currentNode.childCount === 1)
			resetKeyAreaModel()
		qmlMetaKeyModel.clear()
		nameTextField.focus = true
	}
}
