function expand(model, itemLoader) {
	if(model.childCount > 0 && !model.childrenHaveNoChildren){
		itemLoader.expanded = !itemLoader.expanded
		model.isExpanded = itemLoader.expanded
		keyAreaView.selection.clear()
	}
}

function mousePressed(mouse, model, itemLoader) {
	if(mouse.button === Qt.LeftButton){
		currentNode = model
		currentItem = itemLoader
		keyAreaSelectedItem = null
		metaAreaModel = null
		editKeyWindow.selectedNode = currentNode
		forceActiveFocus()

		if (currentNode !== null){
			if(currentNode.childCount > 0 && currentNode.childrenHaveNoChildren)
				keyAreaModel = currentNode.children
			else
				keyAreaModel = null
		}
	}
	else if(mouse.button === Qt.RightButton)
		treeContextMenu.popup()
}

function getOpacity(model) {
	if(model.childCount > 0 && !model.childrenHaveNoChildren)
		return 1
	return 0
}

function getExpanded(model) {
	if(model.isExpanded && model.childrenHaveNoChildren)
		return false
	return model.isExpanded
}
