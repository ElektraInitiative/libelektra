import QtQuick 2.2
import QtQuick.Dialogs 1.1

FileDialog {

	title: qsTr("Export Configuration to File")
	selectExisting: false

	onAccepted: {
		externTreeModel.exportConfiguration(treeView.currentNode.node, "dump", exportDialog.fileUrl)
	}
}
