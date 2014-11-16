import QtQuick 2.2

WizardTemplate {
	id: page2

	wizardText.text: qsTr("Please provide a mount point for the backend. For a cascading mount point start with \"/\".\n\nAlready used are:  " + usedNames)
	label.text: qsTr("Mount point:  ")

	Component.onCompleted: textField.text = (backend.length > 1 ? backend[1] : "")
	buttonRow.backButton.onClicked: loader.source = "Page1.qml"
	buttonRow.finishButton.enabled: false
	buttonRow.nextButton.onClicked: {

		if(textField.text !== ""){
			loader.source = "Page3.qml"
			backend[1] = textField.text
		}
		else
			showMessage(qsTr("No mountpoint provided"), qsTr("Please provide a mountpoint."), "", "", "w")
	}
	usedNames: externTreeModel.mountPoints()
}
