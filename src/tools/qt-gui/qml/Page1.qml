import QtQuick 2.2

WizardTemplate {
	id: page1

	wizardText.text: qsTr("Welcome to the Mounting Wizard! Here you can create and mount backends.\n\n" +
						  "Please provide a mount point for the backend. For a cascading mount point start with \"/\".\n\n" +
						  "Already used are:  " + wizardLoader.usedNames)

	label.text: qsTr("Mount point:  ")

	buttonRow.finishButton.visible: false
	buttonRow.nextButton.enabled: textField.text !== ""
	buttonRow.nextButton.onClicked: {
		guiBackend.createBackend(textField.text)

		if(!error)
			loader.source = "Page2.qml"
	}
}
