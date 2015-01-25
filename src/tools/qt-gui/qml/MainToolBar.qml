import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.0

ToolBar {

	RowLayout {
		id:tbLayout

		anchors.fill: parent

		ToolButton {
			id:tbNew
			iconSource: "icons/document-new.png"
			enabled: newKeyAction.enabled
			menu: Menu {
				MenuItem {
					action: newKeyAction
				}
				MenuItem {
					action: newArrayAction
				}
			}
		}
		ToolButton {
			id:tbDelete
			action: deleteAction
			HelpArea {
				helpText: "This is the delete button."
			}
		}
		ToolButton {
			id:tbImport
			action: importAction
			HelpArea {
				helpText: "This is the import button."
			}
		}
		ToolButton {
			id:tbExport
			action: exportAction
			HelpArea {
				helpText: "This is the export button."
			}
		}
		ToolButton {
			id:tbUndo
			action: undoAction
			HelpArea {
				helpText: "This is the undo button."
			}
		}
		ToolButton {
			id: tbUndoAll

			implicitWidth: defaultMargins
			enabled: undoAction.enabled
			menu: Menu {
				MenuItem {
					action: undoAllAction
				}
			}
		}
		ToolButton {
			id:tbRedo
			action: redoAction
			HelpArea {
				helpText: "This is the redo button."
			}
		}
		ToolButton {
			id: tbRedoAll

			implicitWidth: defaultMargins
			enabled: redoAction.enabled
			menu: Menu {
				MenuItem {
					action: redoAllAction
				}
			}
		}
		ToolButton {
			id:tbSynchronize
			action: synchronizeAction
			HelpArea {
				helpText: "This is the synchronize button."
			}
		}
		Item {
			Layout.fillWidth: true
			height: tbNew.height
		}
		Image {
			id: searchLogo
			source: "icons/edit-find.png"
		}
		SearchField {
			id: searchField

			implicitWidth: keyAreaWidth - searchLogo.implicitWidth - defaultMargins
			focus: true
			onAccepted: {
				if(text !== ""){
					searchResultsListView.model = treeView.treeModel.find(text)
					searchResultsListView.currentIndex = -1
					searchResultsListView.forceActiveFocus()
					searchResultsColorAnimation.running = true
				}
			}
			HelpArea {
				helpText: "This is the search field."
			}
		}
	}
}

