import QtQuick 2.2

WizardTemplate {
	id: page3
	wizardText.text: qsTr("Please enter a path to a file in the filesystem. This file is used by all plugins of this backend as fallback. For user or cascading mountpoints it must be a relative path. The actual path will be located dynamically by the resolver plugin.")
	label.text: qsTr("Path: ")

	buttonRow.backButton.onClicked: loader.source = "Page2.qml"
	buttonRow.finishButton.enabled: false
	buttonRow.nextButton.onClicked: loader.source = "Page4.qml"
	buttonVisible: true
}
