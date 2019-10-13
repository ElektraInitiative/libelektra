//puts key to clipboard when cut key command for a key without children is executed
function cutKey() {
  //needed to mark the node
  keyAreaView.keyAreaCopyIndex = keyAreaView.currentRow;
  keyAreaView.currentNodePath = treeView.currentNode.path;

  undoManager.putToClipboard(
    "cutKey",
    keyAreaSelectedItem.parentModel,
    keyAreaSelectedItem.index
  );
  isPasted = false;
}

//puts key to clipboard when cut key command for a key with children is executed
function cutBranch() {
  treeView.treeAreaCopyIndex = treeView.currentNode.index;
  keyAreaView.currentNodePath = treeView.currentNode.path;

  undoManager.putToClipboard(
    "cutBranch",
    treeView.currentNode.parentModel,
    treeView.currentNode.index
  );
  isPasted = false;
}

//puts key to clipboard when copy key command for a key without children is executed
function copyKey() {
  //needed to mark the node
  keyAreaView.keyAreaCopyIndex = keyAreaView.currentRow;
  keyAreaView.currentNodePath = treeView.currentNode.path;

  undoManager.putToClipboard(
    "copyKey",
    keyAreaSelectedItem.parentModel,
    keyAreaSelectedItem.index
  );
}

//puts key to clipboard when copy key command for a key with children is executed
function copyBranch() {
  //needed to mark the node
  treeView.treeAreaCopyIndex = treeView.currentNode.index;
  treeView.currentNodePath = treeView.currentNode.path;

  undoManager.putToClipboard(
    "copyBranch",
    treeView.currentNode.parentModel,
    treeView.currentNode.index
  );
}

//paste key that is stored in the clipboard to the new parent key
function paste() {
  if (undoManager.clipboardType === "copyKey") {
    undoManager.createCopyKeyCommand(
      treeView.currentNode.parentModel,
      treeView.currentNode.index
    );
    keyAreaView.keyAreaCopyIndex = -1;
    keyAreaView.currentNodePath = "";
  } else if (undoManager.clipboardType === "copyBranch") {
    undoManager.createCopyKeyCommand(
      treeView.currentNode.parentModel,
      treeView.currentNode.index
    );
    refreshModel(treeView.treeModel);
  } else if (undoManager.clipboardType === "cutKey") {
    keyAreaView.keyAreaCopyIndex = -1;
    keyAreaView.currentNodePath = "";

    if (!isPasted) {
      undoManager.createCutKeyCommand(
        treeView.currentNode.parentModel,
        treeView.currentNode.index
      );
      isPasted = true;
    } else {
      undoManager.createCopyKeyCommand(
        treeView.currentNode.parentModel,
        treeView.currentNode.index
      );
    }
  } else if (undoManager.clipboardType === "cutBranch") {
    if (!isPasted) {
      undoManager.createCutKeyCommand(
        treeView.currentNode.parentModel,
        treeView.currentNode.index
      );
      isPasted = true;
    } else {
      undoManager.createCopyKeyCommand(
        treeView.currentNode.parentModel,
        treeView.currentNode.index
      );
    }

    refreshModel(treeView.treeModel);
  }
}

//deletes key when delete key command for a key without children is executed
function deleteKey() {
  var cr = keyAreaView.currentRow;

  undoManager.createDeleteKeyCommand(
    "deleteKey",
    keyAreaSelectedItem.parentModel,
    keyAreaSelectedItem.index
  );

  if (keyAreaView.rowCount > 0 && cr > 0) {
    keyAreaView.currentRow = Math.min(cr - 1, keyAreaView.rowCount - 1);
    updateKeyAreaSelection();
  } else {
    keyAreaSelectedItem = null;
    keyAreaView.selection.clear();
    treeView.treeModel.refresh();
  }
}

//deletes key when delete key command for a key with children is executed
function deleteBranch(treeModel) {
  undoManager.createDeleteKeyCommand(
    "deleteBranch",
    treeModel.currentNode.parentModel,
    treeModel.currentNode.index
  );

  treeModel.treeModel.refresh();
  treeModel.currentNode = null;
}

//deletes key when delete key command for a key located in the search results is executed
function deleteSearchResult() {
  var ci = searchResultsListView.currentIndex;

  if (searchResultsSelectedItem !== null) {
    if (searchResultsSelectedItem.childCount > 0)
      undoManager.createDeleteKeyCommand(
        "deleteSearchResultsBranch",
        searchResultsSelectedItem.parentModel,
        searchResultsSelectedItem.parentModel.getIndexByName(
          searchResultsSelectedItem.name
        )
      );
    else
      undoManager.createDeleteKeyCommand(
        "deleteSearchResultsKey",
        searchResultsSelectedItem.parentModel,
        searchResultsSelectedItem.parentModel.getIndexByName(
          searchResultsSelectedItem.name
        )
      );

    undoManager.createDeleteKeyCommand(
      "deleteSearchResultsKey",
      searchResultsListView.model,
      searchResultsSelectedItem.index
    );

    if (searchResultsListView.model.rowCount() > 0) {
      searchResultsListView.currentIndex = Math.min(
        ci--,
        searchResultsListView.model.rowCount() - 1
      );
      searchResultsSelectedItem = searchResultsListView.model.get(
        searchResultsListView.currentIndex
      );
    } else searchResultsSelectedItem = null;
  }
}

//refreshes the key area view
function updateKeyAreaSelection() {
  keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow);
  editKeyWindow.selectedNode = keyAreaSelectedItem;

  keyAreaView.model.refresh();
  keyAreaView.selection.clear();
  keyAreaView.selection.select(keyAreaView.currentRow);
  keyAreaView.forceActiveFocus();
}

//refreshes a treeview model and preserves the current selected node
function refreshModel(treeModel) {
  var currentModel = treeView.currentNode.parentModel;
  var index = treeView.currentNode.index;
  treeModel.refresh();
  treeView.currentNode = currentModel.get(index);
}
