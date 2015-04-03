import QtQuick 2.3
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.2

MenuBar {

	Menu {
		id:dbdatabase
		title: qsTr("&Database")

		MenuItem {
			id:dbImport
			action: importAction
		}
		MenuItem {
			id:dbExport
			action: exportAction
		}

		MenuSeparator{}

		MenuItem {
			id:dbCreateBackend
			action: mountBackendAction
		}
		MenuItem {
			id:dbUnmountBackend
			action: unmountBackendAction
		}

		MenuSeparator{}

		MenuItem {
			id: dbSynchronize
			action: synchronizeAction
		}

		MenuSeparator{}

		MenuItem {
			id: dbPluginInfo
			action: pluginInfoAction
		}

		MenuSeparator{}

		MenuItem {
			id:dbExit
			action: quitAction
		}
	}

	Menu {
		id:edit
		title: qsTr("&Edit")

		MenuItem {
			id:edUndo
			action: undoAction
		}
		MenuItem {
			id:edRedo
			action: redoAction
		}

		MenuSeparator{}

		Menu {
			id:edNew
			title: qsTr("New")

			MenuItem {
				id:edNewKey
				action: newKeyAction
			}
			MenuItem {
				id:edNewArray
				action: newArrayAction
			}
		}

		MenuItem {
			id:edEdit
			action: editAction
		}

		MenuSeparator{}

		MenuItem {
			id:edCut
			action: cutAction
		}
		MenuItem {
			id:edCopy
			action: copyAction
		}
		MenuItem {
			id:edPaste
			action: pasteAction
		}
		MenuItem {
			id:edDelete
			action: deleteAction
		}
	}

	Menu {
		id: settings
		title: qsTr("Settings")

		MenuItem {
			action: chooseColorAction
		}
	}

	Menu {
		id: help
		title: qsTr("&Help")
		MenuItem {
			action: whatsThisAction
		}
		MenuSeparator{}
		MenuItem {
			action: aboutAction
		}
	}
}


