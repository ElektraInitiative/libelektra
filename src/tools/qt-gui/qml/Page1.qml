import QtQuick 2.2

WizardTemplate {
	id: page1

	wizardText.text: qsTr("Welcome to the Mounting Wizard! Here you can create and mount backends.\n\n" +
						  "Please provide a mount point for the backend. For a cascading mount point start with \"/\".\n\n" +
						  "Already used are:  " + usedNames)

	label.text: qsTr("Mount point:  ")

	Component.onCompleted: textField.text = (backend.length > 0 ? backend[0] : "")
	buttonRow.backButton.visible: false
	buttonRow.finishButton.visible: false
	buttonRow.nextButton.enabled: textField.text !== ""
	buttonRow.nextButton.onClicked: {
		loader.source = "Page2.qml"
		backend[0] = textField.text
	}
	usedNames: externTreeModel.mountPoints()
}
