import QtQuick 2.2
import QtQuick.Window 2.0
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1

ApplicationWindow {

	flags: Qt.SplashScreen
	modality: Qt.ApplicationModal | Qt.WindowStaysOnTopHint
	color: activePalette.alternateBase
	visible: true
	width: splashlogo.implicitWidth + splashTitle.contentWidth + 3*margins
	height:splashlogo.implicitHeight + spacer.implicitHeight + progressbarLabel.implicitHeight + progressbar.implicitHeight + 3*margins

	property int margins: 16

	SystemPalette {
		id: activePalette
		colorGroup: SystemPalette.Active
	}

	Connections {
		target: externTreeModel

		onUpdateProgress: {
			progressbar.setValue(value)
		}
		onFinished: {
			mainLoader.source = "main.qml"
		}
	}

	ColumnLayout {
		anchors.fill: parent
		anchors.margins: margins

		RowLayout {
			spacing: margins

			Image {
				id:splashlogo

				source: "icons/elektra-logo.png"
			}
			Text {
				id: splashTitle

				Layout.fillWidth: true
				anchors.verticalCenter: splashlogo.verticalCenter
				text: "Elektra Editor"
				font.weight: Font.Black
				font.pointSize: 26
				font.capitalization: Font.SmallCaps
				color: activePalette.text
			}
		}
		Item {
			id: spacer

			Layout.fillWidth: true
			implicitHeight: 2*margins
		}
		Label {
			id: progressbarLabel

			text: qsTr("Please wait, loading configuration ...")
		}
		ProgressBar {
			id: progressbar

			Layout.fillWidth: true
			maximumValue: 100
		}
	}
	Loader {
		id: mainLoader

		anchors.fill: parent
		//Asynchronous loading destroys the TableView
		//https://bugreports.qt-project.org/browse/QTBUG-36826
		asynchronous: false//true
		visible: status == Loader.Ready
		onLoaded: close()
	}
}
