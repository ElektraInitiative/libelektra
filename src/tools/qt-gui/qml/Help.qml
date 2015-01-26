import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1
import QtQuick.Window 2.0

Rectangle {
	id: help

	property var	helpText
	property var	defaultMargins
	property int	fadeInDelay: 250
	property int	fadeOutDelay: fadeInDelay

	width: text.width + defaultMargins
	height: text.height + defaultMargins
	color: inActivePalette.base
	border.color: inActivePalette.windowText

	Text {
		id: text

		x: help.defaultMargins
		y: help.defaultMargins

		anchors.centerIn: parent

		color: inActivePalette.windowText

		text: helpText
	}

	function show() {
		state = "SHOW"
	}

	function hide() {
		state = "HIDE"
	}

	states: [
		State {
			name: "SHOW"
			PropertyChanges {
				target: help
				opacity: 1
			}
		},
		State {
			name: "HIDE"
			PropertyChanges {
				target: help
				opacity: 0
			}
		}
	]

	transitions: [
		Transition {
			to: "SHOW"
			NumberAnimation {
				target: help
				property: "opacity"
				duration: fadeInDelay
			}
		},
		Transition {
			to: "HIDE"
			NumberAnimation {
				target: help
				property: "opacity"
				duration: fadeOutDelay
			}
		}
	]

	SystemPalette {
		id: inActivePalette
		colorGroup: SystemPalette.Inactive
	}
	SystemPalette {
		id: activePalette
		colorGroup: SystemPalette.Active
	}
}

