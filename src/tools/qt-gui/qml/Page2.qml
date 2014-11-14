import QtQuick 2.2

WizardTemplate {
	id: page2
	wizardText.text: qsTr("Please provide a mount point for the backend. For a cascading mount point start with \"/\".\n\nAlready used are: " + usedNames)
	label.text: qsTr("Mount point: ")

	buttonRow.backButton.onClicked: loader.source = "Page1.qml"
	buttonRow.finishButton.enabled: false
	buttonRow.nextButton.onClicked: loader.source = "Page3.qml"
	usedNames: "system/elektra, user/mountpoints"
}
