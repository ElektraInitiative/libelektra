import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {
	Menu {
		id:kcmNew

		title: qsTr("New")


		MenuItem {
			id:kcmNewKey

			action: newKeyAction
			text: qsTr("Create Key here ...")
		}
		MenuItem {
			id:kcmNewArray

			action: newArrayAction
			text: qsTr("Create Array Entry here ...")
		}
		MenuItem {
			id:kcmNewKeyBelow

			property string src: "keyBelow"

			action: newKeyAction
			text: qsTr("Create Key below ...")
		}
		MenuItem {
			id:kcmNewArrayBelow

			property string src: "arrBelow"

			action: newArrayAction
			text: qsTr("Create Array Entry below ...")
		}
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

