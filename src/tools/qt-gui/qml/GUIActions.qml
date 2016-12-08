import QtQuick 2.2
import QtQuick.Controls 1.2
import "MainFunctions.js" as MFunctions
import "HelperFunctions.js" as Helper

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
	property alias toggleViewerAction: toggleViewerAction

	Action {
		id: newKeyAction

		text: qsTr("&Key ...")
		iconSource: "icons/document-new.png"
		iconName: Helper.useIcon("document-new")
		tooltip: qsTr("New Key")
		enabled: treeView.currentItem !== null && !toggleViewerAction.checked

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
		iconName: Helper.useIcon("view-grid")
		tooltip: qsTr("New Array Entry")
		enabled: treeView.currentItem !== null && !toggleViewerAction.checked

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
		iconName: Helper.useIcon("user-trash")
		tooltip: qsTr("Delete")
		shortcut: StandardKey.Delete
		enabled: !(searchResultsSelectedItem === null && treeView.currentNode === null && keyAreaSelectedItem === null) && !toggleViewerAction.checked
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
		iconName: Helper.useIcon("document-import")
		tooltip: qsTr("Import Configuration")
		enabled: treeView.currentItem !== null && !toggleViewerAction.checked
		onTriggered: importDialog.show()
	}

	Action {
		id: exportAction

		text: qsTr("E&xport Configuration ... ")
		iconSource: "icons/export.png"
		iconName: Helper.useIcon("document-export")
		tooltip: qsTr("Export Configuration")
		enabled: treeView.currentItem !== null && !toggleViewerAction.checked
		onTriggered: {
			exportDialog.nameFilters = guiBackend.nameFilters()
			exportDialog.open()
		}
	}

	Action {
		id: undoAction

		text: qsTr("Undo")
		iconSource: "icons/edit-undo.png"
		iconName: Helper.useIcon("edit-undo")
		tooltip: qsTr("Undo")
		shortcut: StandardKey.Undo
		enabled: undoManager.canUndo && !toggleViewerAction.checked
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
		enabled: undoManager.canUndo && !toggleViewerAction.checked
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
		iconName: Helper.useIcon("edit-redo")
		tooltip: qsTr("Redo")
		shortcut: StandardKey.Redo
		enabled: undoManager.canRedo && !toggleViewerAction.checked
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
		enabled: undoManager.canRedo && !toggleViewerAction.checked
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
		iconName: Helper.useIcon("view-refresh")
		tooltip: qsTr("Synchronize")
		shortcut: StandardKey.Refresh
		enabled: !toggleViewerAction.checked
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
		iconName: Helper.useIcon("list-add")
		tooltip: qsTr("Mount Backend")
		enabled: !toggleViewerAction.checked
		onTriggered: {
			wizardLoader.usedNames = guiBackend.mountPoints()
			wizardLoader.show()
		}
	}

	Action {
		id: unmountBackendAction

		text: qsTr("&Unmount Backend ...")
		iconSource: "icons/unmount.png"
		iconName: Helper.useIcon("list-remove")
		tooltip: qsTr("Unmount Backend")
		enabled: !toggleViewerAction.checked
		onTriggered: {
			unmountBackendWindow.mountedBackendsView.model = treeView.treeModel.mountedBackends()
			unmountBackendWindow.mountedBackendsView.currentIndex = -1
			unmountBackendWindow.show()
		}
	}

	Action {
		id: editAction

		iconSource: "icons/edit-rename.png"
		iconName: Helper.useIcon("edit-rename")
		text: qsTr("Edit ...")
		tooltip: qsTr("Edit")
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null && searchResultsSelectedItem === null) && !toggleViewerAction.checked

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
		iconName: Helper.useIcon("edit-cut")
		text: qsTr("Cut")
		tooltip: qsTr("Cut")
		shortcut: StandardKey.Cut
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null) && !toggleViewerAction.checked

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
		iconName: Helper.useIcon("edit-copy")
		text: qsTr("Copy")
		tooltip: qsTr("Copy")
		shortcut: StandardKey.Copy
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null) && !toggleViewerAction.checked

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
		iconName: Helper.useIcon("edit-paste")
		text: qsTr("Paste")
		tooltip: qsTr("Paste")
		shortcut: StandardKey.Paste
		enabled: undoManager.canPaste && !toggleViewerAction.checked

		onTriggered: MFunctions.paste()
	}

	Action {
		id: aboutAction

		text: qsTr("About E&lektra Editor")
		iconSource: "icons/elektra.png"
		iconName: Helper.useIcon("elektra")
		onTriggered: aboutWindow.show()
	}

	Action {
		id: whatsThisAction

		text: qsTr("What's This?")
		iconSource: "icons/whats_this.png"
		iconName: Helper.useIcon("help-contextual")
		onTriggered: helpMode ? helpMode = false : helpMode = true
		shortcut: "Shift+F1"
	}

	Action {
		id: pluginInfoAction
		text: qsTr("Show &Plugin Info ...")
		iconSource: "icons/help-about.png"
		iconName: Helper.useIcon("help-about")
		onTriggered: pluginInfo.show()
	}

	Action {
		id: quitAction
		text: qsTr("Quit")
		iconSource: "icons/application-exit.png"
		iconName: Helper.useIcon("application-exit")
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
		iconName: Helper.useIcon("configure")
		onTriggered: appearanceSettingsWindow.show()
	}

	Action {
		id: toggleViewerAction
		checkable: true
		checked: guiSettings.viewermode
		onCheckedChanged: {
			guiSettings.viewermode = checked

			if(undoManager.isClean())
				guiSettings.setKDB()
		}
		text: qsTr("Viewermode")
	}
}

