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
	property int	fadeOutDelay: fadeInDelay
	property int	maxWidth: 0
	property int	metaHeight: 0

	width: Math.max(maxWidth + 2*defaultMargins, keyText.paintedWidth + 2*defaultMargins)
	height: (metaHeight > 0) ? (metaHeight +  keyText.paintedHeight + 3*defaultMargins) : (keyText.paintedHeight + 2*defaultMargins)
	color: inActivePalette.base

	Column {
		id: layout

		anchors.fill: parent
		anchors.centerIn: parent
		anchors.margins: defaultMargins
		spacing: defaultMargins

		Row {
			Label {
				id: keyText

				wrapMode: Text.Wrap
				text: name + " : " + value
			}
		}
		ListView {
			id: metaView

			width: parent.width
			height: metaHeight

			delegate: metaDelegate
		}
	}

	Component {
		id: metaDelegate

		Rectangle {
			width: metaView.width
			height: metaText.paintedHeight
			color: inActivePalette.base

			Label {
				id: metaText

				text: name + " : " + value
				wrapMode: Text.Wrap

				Component.onCompleted: {
					if(paintedWidth > maxWidth)
						maxWidth = paintedWidth

					metaHeight += paintedHeight
				}
			}
		}
	}

	function show() {
		state = "showing"
	}

	function hide() {
		state = "hidden"
	}

	states: [
		State {
			name: "showing"
			PropertyChanges {
				target: tooltip
				opacity: 1
			}
		},
		State {
			name: "hidden"
			PropertyChanges {
				target: tooltip
				opacity: 0
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
		id: inActivePalette
		colorGroup: SystemPalette.Inactive
	}
	SystemPalette {
		id: activePalette
		colorGroup: SystemPalette.Active
	}
}

