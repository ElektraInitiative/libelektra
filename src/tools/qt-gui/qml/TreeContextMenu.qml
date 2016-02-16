import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {
	Menu {
		id:tcmNew

		title: qsTr("New")

		MenuItem {
			id:tcmNewKey

			action: guiActions.newKeyAction
		}
		MenuItem {
			id:tcmNewArray

			action: guiActions.newArrayAction
		}
	}
	MenuItem {
		id: tcmEdit

		action: guiActions.editAction
	}

	MenuSeparator{}

	MenuItem{
		id: tcmImport

		action: guiActions.importAction
	}
	MenuItem{
		id: tcmExport

		action: guiActions.exportAction
	}

	MenuSeparator{}

	MenuItem {
		id: tcmCut

		action: guiActions.cutAction
	}
	MenuItem {
		id: tcmCopy

		action: guiActions.copyAction
	}
	MenuItem {
		id: tcmPaste

		action: guiActions.pasteAction
	}
	MenuItem {
		id:tcmDelete

		action: guiActions.deleteAction
	}
}

