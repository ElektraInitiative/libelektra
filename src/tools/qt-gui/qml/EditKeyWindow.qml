import QtQuick 2.2

KeyWindow {

	title: qsTr("Edit Key")

	keyName: selectedNode === null ? "" : selectedNode.name
	keyValue: (selectedNode === null || selectedNode.value === undefined) ? "" : selectedNode.value

	function populateMetaArea() {
		for(var i = 0; i < selectedNode.metaValue.rowCount(); i++){
			qmlMetaKeyModel.append({"metaName" : selectedNode.metaValue.get(i).name, "metaValue" : selectedNode.metaValue.get(i).value})
		}
	}

	function editAccepted() {

		var metaData = {}
		var index

		if(accessFromSearchResults)
			index = selectedNode.parentModel.getIndexByName(selectedNode.name)
		else
			index = selectedNode.index

		//collect metadata in a map
		for(var i = 0; i < qmlMetaKeyModel.count; i++){
			metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue
		}

		//create undo command
		if(isEdited){
			var data = [keyName.toString(), keyValue.toString(), selectedNode.metaValue,
						nameTextField.text, valueTextField.text, metaData]

			undoManager.createEditKeyCommand(selectedNode.parentModel, index, data)
		}

		if(!error){
			qmlMetaKeyModel.clear()
			selectedNode = null

			if(accessFromSearchResults){
				searchResultsListView.model.refresh()
				searchResultsSelectedItem = searchResultsListView.model.get(searchResultsListView.currentIndex)
			}

			visible = false
			accessFromSearchResults = false
			nameTextField.undo()
			valueTextField.undo()
			nameTextField.readOnly = false
			nameTextField.textColor = activePalette.text

			qmlMetaKeyModel.clear()
			selectedNode = null

			if(keyAreaModel !== null)
				keyAreaSelectedItem = keyAreaModel.get(keyAreaView.currentRow)
		}
	}
}
