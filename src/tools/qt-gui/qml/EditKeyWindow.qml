import QtQuick 2.2

KeyWindow {

	title: qsTr("Edit Key")

    	keyName: (selectedNode === null || selectedNode.name === undefined) ? "" : selectedNode.isNamespaceRoot ? selectedNode.name + "/" : selectedNode.name
	keyValue: (selectedNode === null || selectedNode.value === undefined) ? "" : selectedNode.value

	function populateMetaArea() {
		if (selectedNode.metaValue){
			for (var i = 0; i < selectedNode.metaValue.rowCount(); i++){
				qmlMetaKeyModel.append({"metaName" : selectedNode.metaValue.get(i).name, "metaValue" : selectedNode.metaValue.get(i).value})
			}
		}
	}

	function editAccepted() {

		var metaData = {}
		var index

		if (accessFromSearchResults)
			index = selectedNode.parentModel.getIndexByName(selectedNode.name)
		else
			index = selectedNode.index

		//collect metadata in a map
		for (var i = 0; i < qmlMetaKeyModel.count; i++){
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue
		}

		//create undo command
		if (isEdited){
			container.clearData()

			container.setOldName(keyName.toString())
			container.setOldValue(keyValue.toString())
			container.setOldMetadata(selectedNode.metaValue)

			container.setNewName(nameTextField.text)
			container.setNewValue(valueTextField.text)
			container.setNewMetadata(metaData)

			undoManager.createEditKeyCommand(selectedNode.parentModel, index, container)
		}

		if (!error){
			qmlMetaKeyModel.clear()
			selectedNode = null

			if (accessFromSearchResults){
				searchResultsListView.model.refresh()
				searchResultsSelectedItem = searchResultsListView.model.get(searchResultsListView.currentIndex)
			}

			visible = false
			accessFromSearchResults = false
			nameTextField.readOnly = false
			nameTextField.textColor = activePalette.text

			qmlMetaKeyModel.clear()

//			if (keyAreaView.model !== null && !accessFromSearchResults){
//				keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
//			}
		}
	}
}
