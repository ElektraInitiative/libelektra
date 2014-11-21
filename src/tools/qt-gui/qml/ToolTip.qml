import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1

BasicRectangle {
	id:tooltip

	property var	name
	property var	value
	property alias	meta: metaView.model
	property var	defaultMargins
	property int	fadeInDelay: 250
	property int	fadeOutDelay: 250
	property bool	autoHide: true
	property alias	autoHideDelay: hideTimer.interval
	property bool	destroyOnHide: true
	property int	maxWidth: 0

	width: Math.max(maxWidth + 2*defaultMargins, keyText.paintedWidth + 2*defaultMargins)
	height: metaView.height*2 + keyText.paintedHeight + 2*defaultMargins
	color: activePalette.button

	MouseArea {
		anchors.fill: parent
		hoverEnabled: true
		preventStealing: true
		propagateComposedEvents: false

		onEntered: {
			hideTimer.stop()
		}
		onClicked: hide()
	}

	ColumnLayout {
		id: layout

		anchors.fill: parent
		anchors.centerIn: parent
		anchors.margins: defaultMargins
		spacing: defaultMargins

		Row {
			Text {
				id: keyText

				color: activePalette.text
				text: name + " : " + value
			}
		}
		ListView {
			id: metaView

			width: parent.width
			height: (keyText.paintedHeight)*model.count()

			delegate: metaDelegate
		}
	}

	Component {
		id: metaDelegate

		Row {
			Text {
				color: activePalette.text
				text: name + " : " + value

				Component.onCompleted: {
					if(paintedWidth > maxWidth)
						maxWidth = paintedWidth
				}
			}
		}
	}

	function show() {
		state = "showing"
		if (hideTimer.running) {
			hideTimer.restart()
		}
	}

	function hide() {
		if (hideTimer.running) {
			hideTimer.stop()
		}
		state = "hidden"
	}

	Timer {
		id: hideTimer
		interval: 3000
		onTriggered: hide()
	}

	states: [
		State {
			name: "showing"
			PropertyChanges {
				target: tooltip
				opacity: 1
			}
			onCompleted: {
				if (autoHide) {
					hideTimer.start()
				}
			}
		},
		State {
			name: "hidden"
			PropertyChanges {
				target: tooltip
				opacity: 0
			}
			onCompleted: {
				if (destroyOnHide) {
					tooltip.destroy()
				}
			}
		}
	]

	transitions: [
		Transition {
			to: "showing"
			NumberAnimation {
				target: tooltip
				property: "opacity"
				duration: fadeInDelay
			}
		},
		Transition {
			to: "hidden"
			NumberAnimation {
				target: tooltip
				property: "opacity"
				duration: fadeOutDelay
			}
		}
	]

	SystemPalette {
		id: activePalette
		colorGroup: SystemPalette.Active
	}
}

