import QtQuick 2.2
import QtQuick.Window 2.0
import QtQuick.Controls 1.3
import QtQuick.Layouts 1.1

Window {
	id: splashscreen

	flags: Qt.SplashScreen
	color: "black"
	visible: true
	height: 250
	width: 500

	Item {
		anchors.fill: parent
		anchors.margins: 16

		Image {
			id:splashlogo

			anchors.left: parent.left
			source: "icons/elektra-logo.png"
		}
		Text {
			id: splashTitle

			anchors.left: splashlogo.right
			anchors.verticalCenter: splashlogo.verticalCenter
			anchors.leftMargin: 16
			text: "Elektra Editor Dummy Splash Screen"
			font.weight: Font.Black
			color: "white"
		}
		Label {
			id: progressbarLabel
			anchors.top: splashlogo.bottom
			anchors.topMargin: progressbar.height*2
			text: "Loading Configuration ..."
		}
		ProgressBar {
			id: progressbar

			anchors.top: progressbarLabel.bottom
			anchors.topMargin: 8
			anchors.left: parent.left
			anchors.right: parent.right
			indeterminate: true
		}
	}

	Loader {
		id: main

		anchors.fill: parent
		asynchronous: true
		visible: Loader.Ready
	}

	PauseAnimation {
		id: fakeLoadingDelay
		duration: 50
		onRunningChanged: {
			if (!running) {
				main.source = "main.qml"
				externTreeModel.populateModel()
				splashscreen.visible = false
			}
		}
	}
	Component.onCompleted: fakeLoadingDelay.start()
}


