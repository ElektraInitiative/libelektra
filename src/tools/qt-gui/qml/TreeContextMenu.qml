import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {
	Menu {
		id:tcmNew

		title: qsTr("New")

		MenuItem {
			id:tcmNewKey

			action: newKeyAction
		}
		MenuItem {
			id:tcmNewArray

			action: newArrayAction
		}
	}
	MenuItem {
		id: tcmEdit

		action: editAction
	}

	MenuSeparator{}

	MenuItem{
		id: tcmImport

		action: importAction
	}
	MenuItem{
		id: tcmExport

		action: exportAction
	}

	MenuSeparator{}

	MenuItem {
		id: tcmCut

		action: cutAction
	}
	MenuItem {
		id: tcmCopy

		action: copyAction
	}
	MenuItem {
		id: tcmPaste

		action: pasteAction
	}
	MenuItem {
		id:tcmDelete

		action: deleteAction
	}
}

