import QtQuick 2.2
import QtQuick.Window 2.1
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1

Window {
	id: wizardLoader

	property string usedNames

	width: Math.ceil(mainWindow.width*0.4)
	height: Math.ceil(mainWindow.height*0.4)

	x: Math.ceil(Screen.desktopAvailableWidth*0.5-width*0.5)
	y: Math.ceil(Screen.desktopAvailableHeight*0.5-height*0.5)

	title: qsTr("Mount Backend")

	color: activePalette.window

	Loader {
		id: loader
		anchors.fill: parent
		source: "Page1.qml"
	}
}
