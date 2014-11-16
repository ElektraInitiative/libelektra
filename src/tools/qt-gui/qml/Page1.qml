import QtQuick 2.2

WizardTemplate {

	id: page1

	wizardText.text: qsTr("Welcome to the Mounting Wizard! Here you can create and mount backends." +
						  "\n\nPlease provide a unique name.\n\nAlready used are: " + usedNames)

	label.text: qsTr("Backend name: ")
	usedNames: " default" + (externTreeModel.mountedBackends().toString() === "empty" ? "" : ", " + externTreeModel.mountedBackends().toString().replace(",", ", "))

	Component.onCompleted: textField.text = (backend.length > 0 ? backend[0] : "")

	buttonRow.backButton.enabled: false
	buttonRow.finishButton.enabled: false
	buttonRow.nextButton.onClicked: {
		if(textField.text !== ""){
			loader.source = "Page2.qml"
			backend[0] = textField.text
		}
		else
			showMessage(qsTr("No name provided"), qsTr("Please provide a unique name."), "", "", "w")
	}
}
