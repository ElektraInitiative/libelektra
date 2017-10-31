import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.2

MenuBar {

	Menu {
		id:dbdatabase
		title: qsTr("&Database")

		MenuItem {
			id:dbImport
			action: guiActions.importAction
		}
		MenuItem {
			id:dbExport
			action: guiActions.exportAction
		}

		MenuSeparator{}

		MenuItem {
			id:dbCreateBackend
			action: guiActions.mountBackendAction
		}
		MenuItem {
			id:dbUnmountBackend
			action: guiActions.unmountBackendAction
		}

		MenuSeparator{}

		MenuItem {
			id: dbSynchronize
			action: guiActions.synchronizeAction
		}

		MenuSeparator{}

		MenuItem {
			id: dbPluginInfo
			action: guiActions.pluginInfoAction
		}

		MenuSeparator{}

		MenuItem {
			id:dbExit
			action: guiActions.quitAction
		}
	}

	Menu {
		id:edit
		title: qsTr("&Edit")

		MenuItem {
			id:edUndo
			action: guiActions.undoAction
		}
		MenuItem {
			id:edRedo
			action: guiActions.redoAction
		}

		MenuSeparator{}

		Menu {
			id:edNew
			title: qsTr("New")

			MenuItem {
				id:edNewKey
				action: guiActions.newKeyAction
			}
			MenuItem {
				id:edNewArray
				action: guiActions.newArrayAction
			}
		}

		MenuItem {
			id:edEdit
			action: guiActions.editAction
		}

		MenuSeparator{}

		MenuItem {
			id:edCut
			action: guiActions.cutAction
		}
		MenuItem {
			id:edCopy
			action: guiActions.copyAction
		}
		MenuItem {
			id:edPaste
			action: guiActions.pasteAction
		}
		MenuItem {
			id:edDelete
			action: guiActions.deleteAction
		}
	}

	Menu {
		id: settings
		title: qsTr("Settings")

		MenuItem {
			action: guiActions.chooseAppearanceAction
		}
		MenuSeparator{}
		MenuItem {
			action: guiActions.toggleViewerAction
		}
	}

	Menu {
		id: help
		title: qsTr("&Help")
		MenuItem {
			action: guiActions.whatsThisAction
		}
		MenuSeparator{}
		MenuItem {
			action: guiActions.aboutAction
		}
	}
}


