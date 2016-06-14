import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1
import "HelperFunctions.js" as Helper

RowLayout {

	anchors {
		bottom: parent.bottom
		right: parent.right
		margins: defaultMargins
	}

	property alias nextButton: nextButton
	property alias cancelButton: cancelButton

	Button {
		id:nextButton

		action: Action {
			text: qsTr("&Next")
			iconSource: "icons/go-next.png"
			iconName: Helper.useIcon("go-next")
		}
	}
	Button {
		id:cancelButton

		action: Action {
			text: qsTr("&Cancel")
			iconSource: "icons/dialog-cancel.png"
			iconName: Helper.useIcon("dialog-cancel")
		}
	}
}
