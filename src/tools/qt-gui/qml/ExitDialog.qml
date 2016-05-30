import QtQuick 2.2
import QtQuick.Dialogs 1.1

MessageDialog {
	icon: StandardIcon.Question
	title: qsTr("Close Elektra Qt Editor")
	text: qsTr("The configuration has been modified.")
	informativeText: qsTr("Do you want to save your changes or discard them?")
	standardButtons: StandardButton.Save | StandardButton.Discard | StandardButton.Cancel
	onAccepted: {
		treeView.treeModel.synchronize()
		Qt.quit()
	}
	onDiscard: {
		if (undoManager.undoText === "import"){
			undoManager.undo()
		}

		Qt.quit()
	}

}
