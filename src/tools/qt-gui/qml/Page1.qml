import QtQuick 2.2

WizardTemplate {
	id: page1

	wizardText.text: qsTr("Welcome to the Mounting Wizard! Here you can create and mount backends. " +
						  "You can only create backends if you have root administration rights.\n\n" +
						  "The name must be a valid keyname or start with / for cascading mountpoints.\n\n" +
						  "Please provide a mount point for the backend.\n\n" +
						  "Already used are: %1").arg(wizardLoader.usedNames)

	label.text: qsTr("Mount point:  ")

	buttonRow.finishButton.visible: false
	buttonRow.nextButton.action.enabled: textField.text !== ""
	buttonRow.nextButton.action.onTriggered: {
		if(textField.text.charAt(0) === "/")
			guiBackend.createBackend(textField.text)
		else
			guiBackend.createBackend("/" + textField.text)

		if(!error)
			loader.source = "Page2.qml"
	}
}
