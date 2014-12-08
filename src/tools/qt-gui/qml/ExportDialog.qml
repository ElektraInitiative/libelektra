import QtQuick 2.2
import QtQuick.Dialogs 1.1

FileDialog {

	title: qsTr("Export Configuration to File")
	selectExisting: false

	onAccepted: {
		var plugin = selectedNameFilter.match(/[A-Z]+/).toString()
		plugin = plugin.toLowerCase()

		externTreeModel.exportConfiguration(treeView.currentNode.parentModel, treeView.currentNode.index, plugin, exportDialog.fileUrl)
	}

}
