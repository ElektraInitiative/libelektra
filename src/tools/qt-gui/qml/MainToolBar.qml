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
		}
		ToolButton {
			id:tbImport
			action: importAction
		}
		ToolButton {
			id:tbExport
			action: exportAction
		}
		ToolButton {
			id:tbUndo
			action: undoAction
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
		}
		Item {
			width: Math.ceil(mainWindow.width*0.3 - 7*tbRedo.width - 9*defaultSpacing - 3*defaultMargins)
			height: tbNew.height
		}
		Image {
			id: searchLogo
			source: "icons/edit-find.png"
		}
		SearchField {
			id: searchField
			Layout.fillWidth: true
			focus: true
			onAccepted: {
				if(text !== ""){
					searchResultsListView.model = externTreeModel.find(text)
					searchResultsListView.currentIndex = -1
					searchResultsListView.forceActiveFocus()
					searchResultsColorAnimation.running = true
				}
			}
		}
	}
}

