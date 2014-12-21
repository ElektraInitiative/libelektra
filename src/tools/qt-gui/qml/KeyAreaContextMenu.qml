import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {

	MenuItem {
		id: kcmNewKey
		action: newKeyAction
	}
	MenuItem {
		id: kcmEdit
		action: editAction
	}
	MenuItem {
		id: kcmCut
		action: cutAction
	}
	MenuItem {
		id: kcmCopy
		action: copyAction
	}
	MenuItem {
		id: kcmPaste
		action: pasteAction
	}
	MenuItem {
		id: kcmDelete
		action: deleteAction
	}
}

