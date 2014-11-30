import QtQuick 2.2
import QtQuick.Dialogs 1.1

FileDialog {

	title: qsTr("Export Configuration to File")
	selectExisting: false

	onAccepted: {
		var plugin = "dump"

		if(selectedNameFilter === "XML (*.xml)" || exportDialog.fileUrl.toString().substr(exportDialog.fileUrl.toString().lastIndexOf("."), 4) === ".xml")
			plugin = "xmltool"
		else if(selectedNameFilter === "INI (*.ini)" || exportDialog.fileUrl.toString().substr(exportDialog.fileUrl.toString().lastIndexOf("."), 4) === ".ini")
			plugin = "ini"

		externTreeModel.exportConfiguration(treeView.currentNode.parentModel, treeView.currentNode.index, plugin, exportDialog.fileUrl)
	}

}
