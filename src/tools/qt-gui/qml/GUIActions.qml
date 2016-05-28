import QtQuick 2.2
import QtQuick.Controls 1.2
import "MainFunctions.js" as MFunctions

Item {
	property alias newKeyAction: newKeyAction
	property alias newArrayAction: newArrayAction
	property alias deleteAction: deleteAction
	property alias importAction: importAction
	property alias exportAction: exportAction
	property alias undoAction: undoAction
	property alias undoAllAction: undoAllAction
	property alias redoAction: redoAction
	property alias redoAllAction: redoAllAction
	property alias synchronizeAction: synchronizeAction
	property alias mountBackendAction: mountBackendAction
	property alias unmountBackendAction: unmountBackendAction
	property alias editAction: editAction
	property alias cutAction: cutAction
	property alias copyAction: copyAction
	property alias pasteAction: pasteAction
	property alias aboutAction: aboutAction
	property alias whatsThisAction: whatsThisAction
	property alias pluginInfoAction: pluginInfoAction
	property alias quitAction: quitAction
	property alias chooseAppearanceAction: chooseAppearanceAction

	Action {
		id: newKeyAction

		text: qsTr("&Key ...")
		iconSource: "icons/document-new.png"
		tooltip: qsTr("New Key")
		enabled: treeView.currentItem !== null
		onTriggered: {

			if(source.src === "keyBelow"){
				newKeyWindow.selectedNode = keyAreaSelectedItem
				newKeyWindow.isBelow = true
			}
			else
				newKeyWindow.selectedNode = treeView.currentNode

			newKeyWindow.show()
		}
	}

	Action {
		id:newArrayAction

		text: qsTr("&Array Entry ...")
		iconSource: "icons/new-array.png"
		tooltip: qsTr("New Array Entry")
		enabled: treeView.currentItem !== null
		onTriggered: {

			if(source.src === "arrBelow"){
				newArrayWindow.selectedNode = keyAreaSelectedItem
				newArrayWindow.isBelow = true
			}
			else
				newArrayWindow.selectedNode = treeView.currentNode

			newArrayWindow.show()
		}
	}

	Action {
		id:deleteAction

		text: qsTr("Delete")
		iconSource: "icons/document-close.png"
		tooltip: qsTr("Delete")
		shortcut: StandardKey.Delete
		enabled: !(searchResultsSelectedItem === null && treeView.currentNode === null && keyAreaSelectedItem === null)
		onTriggered: {
			if(searchResultsSelectedItem !== null)
				MFunctions.deleteSearchResult()
			else if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.deleteBranch(treeView)
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.deleteKey()
		}
	}

	Action {
		id: importAction

		text: qsTr("&Import Configuration ... ")
		iconSource: "icons/import.png"
		tooltip: qsTr("Import Configuration")
		enabled: treeView.currentItem !== null
		onTriggered: importDialog.show()
	}

	Action {
		id: exportAction

		text: qsTr("E&xport Configuration ... ")
		iconSource: "icons/export.png"
		tooltip: qsTr("Export Configuration")
		enabled: treeView.currentItem !== null
		onTriggered: {
			exportDialog.nameFilters = guiBackend.nameFilters()
			exportDialog.open()
		}
	}

	Action {
		id: undoAction

		text: qsTr("Undo")
		iconSource: "icons/edit-undo.png"
		tooltip: qsTr("Undo")
		shortcut: StandardKey.Undo
		enabled: undoManager.canUndo
		onTriggered: {
			if(undoManager.undoText === "deleteBranch"){
				undoManager.undo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.undoText === "deleteKey"){
				undoManager.undo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.undoText === "deleteSearchResultsKey" || undoManager.undoText === "deleteSearchResultsBranch"){
				undoManager.undo()
				undoManager.undo()
			}
			else if(undoManager.undoText === "copyBranch"){
				undoManager.undo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.undoText === "cutBranch"){
				undoManager.undo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.undoText === "import"){
				undoManager.undo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.undoText === "newBranch"){
				undoManager.undo()
				treeView.treeModel.refresh()
				keyAreaView.selection.clear()
			}
			else if(undoManager.undoText === "newKey"){
				undoManager.undo()
				keyAreaView.selection.clear()
				keyAreaView.currentRow = -1
			}
			else{
				undoManager.undo()
				if(searchResultsListView.model !== null && searchResultsListView.model !== undefined)
					searchResultsListView.model.refresh()
			}

			treeView.updateIndicator()
		}

	}

	Action {
		id: undoAllAction

		text: qsTr("Undo All")
		tooltip: qsTr("Undo All")
		enabled: undoManager.canUndo
		onTriggered: {
			//cannot use UndoStack::setIndex() because View-Updates would get lost
			for(var i = undoManager.index(); i > undoManager.cleanIndex(); i--)
				undoAction.trigger()

			treeView.treeModel.refresh()
		}
	}

	Action {
		id: redoAction

		text: qsTr("Redo")
		iconSource: "icons/edit-redo.png"
		tooltip: qsTr("Redo")
		shortcut: StandardKey.Redo
		enabled: undoManager.canRedo
		onTriggered: {
			if(undoManager.redoText === "deleteBranch"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.redoText === "deleteKey"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.redoText === "deleteSearchResultsKey" || undoManager.redoText === "deleteSearchResultsBranch"){
				undoManager.redo()
				undoManager.redo()
			}
			else if(undoManager.redoText === "copyBranch"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.redoText === "cutBranch"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.redoText === "import"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else if(undoManager.redoText === "newBranch"){
				undoManager.redo()
				treeView.treeModel.refresh()
			}
			else{
				undoManager.redo()
				if(searchResultsListView.model !== null && searchResultsListView.model !== undefined)
					searchResultsListView.model.refresh()
			}
			treeView.updateIndicator()
		}

	}

	Action {
		id: redoAllAction

		text: qsTr("Redo All")
		tooltip: qsTr("Redo All")
		enabled: undoManager.canRedo
		onTriggered: {
			//cannot use UndoStack::setIndex() because View-Updates would get lost
			for(var i = undoManager.index(); i < undoManager.count(); i++)
				redoAction.trigger()

			treeView.treeModel.refresh()
		}
	}

	Action {
		id: synchronizeAction

		text: qsTr("Synchronize")
		iconSource: "icons/view-refresh.png"
		tooltip: qsTr("Synchronize")
		shortcut: StandardKey.Refresh
		onTriggered: {
			treeView.treeModel.synchronize()
			undoManager.setClean()
			treeView.treeModel.refresh()
		}
	}

	Action {
		id: mountBackendAction

		text: qsTr("&Mount Backend ...")
		iconSource: "icons/mount.png"
		tooltip: qsTr("Mount Backend")
		onTriggered: {
			wizardLoader.usedNames = guiBackend.mountPoints()
			wizardLoader.show()
		}
	}

	Action {
		id: unmountBackendAction

		text: qsTr("&Unmount Backend ...")
		iconSource: "icons/unmount.png"
		tooltip: qsTr("Unmount Backend")
		onTriggered: {
			unmountBackendWindow.mountedBackendsView.model = treeView.treeModel.mountedBackends()
			unmountBackendWindow.mountedBackendsView.currentIndex = -1
			unmountBackendWindow.show()
		}
	}

	Action {
		id: editAction

		iconSource: "icons/edit-rename.png"
		text: qsTr("Edit ...")
		tooltip: qsTr("Edit")
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null && searchResultsSelectedItem === null)

		onTriggered: {
			if(editKeyWindow.accessFromSearchResults){
				editKeyWindow.selectedNode = searchResultsListView.model.get(searchResultsListView.currentIndex)
			}
			editKeyWindow.populateMetaArea()
			editKeyWindow.show()
		}
	}

	Action {
		id: cutAction

		iconSource: "icons/edit-cut.png"
		text: qsTr("Cut")
		tooltip: qsTr("Cut")
		shortcut: StandardKey.Cut
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null)

		onTriggered: {
			if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.cutBranch()
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.cutKey()
		}
	}

	Action {
		id: copyAction

		iconSource: "icons/edit-copy.png"
		text: qsTr("Copy")
		tooltip: qsTr("Copy")
		shortcut: StandardKey.Copy
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null)

		onTriggered: {
			if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.copyBranch()
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.copyKey()
		}
	}

	Action {
		id: pasteAction

		iconSource: "icons/edit-paste.png"
		text: qsTr("Paste")
		tooltip: qsTr("Paste")
		shortcut: StandardKey.Paste
		enabled: undoManager.canPaste

		onTriggered: MFunctions.paste()
	}

	Action {
		id: aboutAction

		text: qsTr("About E&lektra Editor")
		iconSource: "icons/elektra-logo.png"
		onTriggered: aboutWindow.show()
	}

	Action {
		id: whatsThisAction

		text: qsTr("What's This?")
		iconSource: "icons/whats_this.png"
		onTriggered: helpMode ? helpMode = false : helpMode = true
		shortcut: "Shift+F1"
	}

	Action {
		id: pluginInfoAction
		text: qsTr("Show &Plugin Info ...")
		iconSource: "icons/help-about.png"
		onTriggered: pluginInfo.show()
	}

	Action {
		id: quitAction
		text: qsTr("Quit")
		iconSource: "icons/application-exit.png"
		onTriggered: {
			if(!undoManager.isClean())
				exitDialog.open()
			else
				Qt.quit()
		}
		shortcut: StandardKey.Quit
	}

	Action {
		id: chooseAppearanceAction
		text: qsTr("Choose Appearance ...")
		iconSource: "icons/color.png"
		onTriggered: appearanceSettingsWindow.show()
	}
}

