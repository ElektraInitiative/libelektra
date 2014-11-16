import QtQuick 2.2

WizardTemplate {
	id: page3

	wizardText.text: qsTr("Please enter a path to a file in the filesystem. This file is used by all plugins of this backend as fallback. " +
						  "For user or cascading mountpoints it must be a relative path. " +
						  "The actual path will be located dynamically by the resolver plugin.")

	label.text: qsTr("Path: ")

	Component.onCompleted: textField.text = (backend.length > 2 ? backend[2] : "")
	buttonRow.backButton.onClicked: loader.source = "Page2.qml"
	buttonRow.finishButton.enabled: false
	buttonRow.nextButton.onClicked:  {

		if(textField.text !== ""){
			loader.source = "Page4.qml"
			backend[2] = textField.text
		}
		else
			showMessage(qsTr("No path provided"), qsTr("Please provide a path."), "", "", "w")
	}
	buttonVisible: true
	fileDialog.onAccepted: textField.text = fileDialog.fileUrl.toString().replace("file://", "")
}
