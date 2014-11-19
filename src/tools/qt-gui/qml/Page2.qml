import QtQuick 2.2

WizardTemplate {
	id: page3

	wizardText.text: qsTr("Please enter a path to a file in the filesystem. This file is used by all plugins of this backend as fallback. " +
						  "For user or cascading mountpoints it must be a relative path. " +
						  "The actual path will be located dynamically by the resolver plugin.")

	label.text: qsTr("Path: ")

	buttonRow.backButton.visible: false
	buttonRow.finishButton.visible: false
	buttonRow.nextButton.enabled: textField.text !== ""
	buttonRow.nextButton.onClicked:  {
		loader.source = "Page3.qml"
		backend[2] = textField.text
	}
	buttonVisible: true
	fileDialog.onAccepted: textField.text = fileDialog.fileUrl.toString().replace("file://", "")
}
