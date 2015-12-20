import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {
	Menu {
		id:kcmNew

		title: qsTr("New")


		MenuItem {
			id:kcmNewKey

			action: guiActions.newKeyAction
			text: qsTr("Create Key here ...")
		}
		MenuItem {
			id:kcmNewArray

			action: guiActions.newArrayAction
			text: qsTr("Create Array Entry here ...")
		}
		MenuItem {
			id:kcmNewKeyBelow

			property string src: "keyBelow"

			action: guiActions.newKeyAction
			text: qsTr("Create Key below ...")
		}
		MenuItem {
			id:kcmNewArrayBelow

			property string src: "arrBelow"

			action: guiActions.newArrayAction
			text: qsTr("Create Array Entry below ...")
		}
	}
	MenuItem {
		id: kcmEdit
		action: guiActions.editAction
	}
	MenuItem {
		id: kcmCut
		action: guiActions.cutAction
	}
	MenuItem {
		id: kcmCopy
		action: guiActions.copyAction
	}
	MenuItem {
		id: kcmPaste
		action: guiActions.pasteAction
	}
	MenuItem {
		id: kcmDelete
		action: guiActions.deleteAction
	}
}

