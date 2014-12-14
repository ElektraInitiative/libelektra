//display an error message dialog
function showMessage(title, text, detailedText) {
	errorDialog.title = title
	errorDialog.text = text
	errorDialog.detailedText = detailedText

	error = true
	errorDialog.show()
}

function cutKey() {
	//console.log("cut Key")
	//needed to mark the node
	keyAreaView.keyAreaCopyIndex = keyAreaView.currentRow
	keyAreaView.currentNodePath = treeView.currentNode.path

	undoManager.putToClipboard("cutKey", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.index)
	isPasted = false
}

function cutBranch() {
	//console.log("cut Branch")
	treeView.treeAreaCopyIndex = treeView.currentNode.index
	keyAreaView.currentNodePath = treeView.currentNode.path

	undoManager.putToClipboard("cutBranch", treeView.currentNode.parentModel, treeView.currentNode.index)
	isPasted = false
}

function copyKey() {
	//console.log("copy Key")
	//needed to mark the node
	keyAreaView.keyAreaCopyIndex = keyAreaView.currentRow
	keyAreaView.currentNodePath = treeView.currentNode.path

	undoManager.putToClipboard("copyKey", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.index)
}

function copyBranch() {
	//console.log("copy Branch")
	//needed to mark the node
	treeView.treeAreaCopyIndex = treeView.currentNode.index
	treeView.currentNodePath = treeView.currentNode.path

	undoManager.putToClipboard("copyBranch", treeView.currentNode.parentModel, treeView.currentNode.index)
}

function paste() {

	if(undoManager.clipboardType === "copyKey"){
		undoManager.createCopyKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
		keyAreaView.keyAreaCopyIndex = -1
		keyAreaView.currentNodePath = ""
		resetKeyAreaModel()
		if(keyAreaSelectedItem === null){
			keyAreaModel.refresh()
		}
	}
	else if(undoManager.clipboardType === "copyBranch"){

		undoManager.createCopyKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
		refreshModel(externTreeModel)
		if(!treeView.currentNode.childrenHaveNoChildren)
			keyAreaModel = null
		else{
			if(keyAreaModel === null)
			keyAreaModel = treeView.currentNode.children
		}
	}
	else if(undoManager.clipboardType === "cutKey"){

		keyAreaView.keyAreaCopyIndex = -1
		keyAreaView.currentNodePath = ""

		if(!isPasted){
			undoManager.createCutKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
			isPasted = true
		}
		else{
			undoManager.createCopyKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
		}

		if(keyAreaSelectedItem === null){
			keyAreaModel.refresh()
		}
	}
	else if(undoManager.clipboardType === "cutBranch"){

		if(!isPasted){
			undoManager.createCutKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
			isPasted = true
		}
		else{
			undoManager.createCopyKeyCommand(treeView.currentNode.parentModel, treeView.currentNode.index)
		}

		refreshModel(externTreeModel)
		if(!treeView.currentNode.childrenHaveNoChildren)
			keyAreaModel = null
		else{
			if(keyAreaModel === null)
			keyAreaModel = treeView.currentNode.children
		}
	}
}

function deleteKey() {
	//		console.log("delete key")
	var cr = keyAreaView.currentRow

	undoManager.createDeleteKeyCommand("deleteKey", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.index)

	metaAreaModel = null
	keyAreaSelectedItem = null
	keyAreaModel.refresh()

	if(keyAreaView.rowCount > 0){
		keyAreaView.currentRow = Math.min(cr--, keyAreaView.rowCount - 1)
		updateKeyAreaSelection()
	}
	else
		keyAreaSelectedItem = null
}

function deleteBranch() {
	//		console.log("delete branch")

	undoManager.createDeleteKeyCommand("deleteBranch", treeView.currentNode.parentModel, treeView.currentNode.index)

	externTreeModel.refresh()
	treeView.currentNode = null
}

function deleteSearchResult(){
	//console.log("delete search result")
	var ci = searchResultsListView.currentIndex

	if(searchResultsSelectedItem !== null){

		if(searchResultsSelectedItem.childCount > 0)
			undoManager.createDeleteKeyCommand("deleteSearchResultsBranch", searchResultsSelectedItem.parentModel, searchResultsSelectedItem.node, searchResultsSelectedItem.parentModel.getIndexByName(searchResultsSelectedItem.name))
		else
			undoManager.createDeleteKeyCommand("deleteSearchResultsKey", searchResultsSelectedItem.parentModel, searchResultsSelectedItem.node, searchResultsSelectedItem.parentModel.getIndexByName(searchResultsSelectedItem.name))

		undoManager.createDeleteKeyCommand("deleteSearchResultsKey", searchResultsListView.model, searchResultsSelectedItem.node, searchResultsSelectedItem.index)

		if(searchResultsListView.model.count() > 0){
			searchResultsListView.currentIndex = Math.min(ci--, searchResultsListView.model.count() - 1)
			searchResultsSelectedItem = searchResultsListView.model.get(searchResultsListView.currentIndex)
		}
		else
			searchResultsSelectedItem = null
	}
}

function updateKeyAreaSelection() {
	keyAreaSelectedItem = keyAreaModel.get(keyAreaView.currentRow)
	editKeyWindow.selectedNode = keyAreaSelectedItem
	metaAreaModel = keyAreaSelectedItem.metaValue

	keyAreaView.selection.clear()
	keyAreaView.selection.select(keyAreaView.currentRow)
	keyAreaView.forceActiveFocus()
}

function resetKeyAreaModel() {
	keyAreaModel = null
	keyAreaModel = treeView.currentNode === null ? null : treeView.currentNode.children
}

function refreshModel(treeModel) {
	var currentModel = treeView.currentNode.parentModel
	var index = treeView.currentNode.index
	treeModel.refresh()
	treeView.currentNode = currentModel.get(index)
}
