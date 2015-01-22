import QtQuick 2.2
import QtQuick.Controls 1.1

Menu {
	Menu {
		id:kcmNew

		title: qsTr("New")

		Menu {
			title: qsTr("Create here ...")

			MenuItem {
				id:kcmNewKey

				action: newKeyAction
			}
			MenuItem {
				id:kcmNewArray

				action: newArrayAction
			}

		}
		Menu {
			title: qsTr("Create below ...")

			MenuItem {
				id:kcmNewKeyBelow

				property string src: "keyBelow"

				action: newKeyAction
			}
			MenuItem {
				id:kcmNewArrayBelow

				property string src: "arrBelow"

				action: newArrayAction
			}
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

