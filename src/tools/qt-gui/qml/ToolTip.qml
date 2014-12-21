import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1
import QtQuick.Window 2.0

BasicRectangle {
	id:tooltip

	property var	name
	property var	value
	property alias	meta: metaView.model
	property var	defaultMargins
	property int	parentWidth
	property int	parentHeight
	property int	fadeInDelay: 250
	property int	fadeOutDelay: fadeInDelay
	property int	keyWidth: 0
	property int	maxWidth: 0
	property int	metaHeight: 0

	width: Math.min(parentWidth - x - defaultMargins, maxWidth + 2*defaultMargins)
	height: (metaHeight > 0) ? Math.min((metaHeight +  keyText.height + 3*defaultMargins), parentHeight - y - defaultMargins) : (keyText.height + 2*defaultMargins)
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
				width: tooltip.width - 2*defaultMargins
				text: name + " : " + value
				clip: true
				Component.onCompleted: {
					if(paintedWidth > maxWidth)
						maxWidth = paintedWidth
				}
			}
		}
		ListView {
			id: metaView

			width: tooltip.width - 2*defaultMargins
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

				width: tooltip.width - 2*defaultMargins
				clip: true
				text: name + " : " + value

				Component.onCompleted: {
					if(paintedWidth > maxWidth)
						maxWidth = paintedWidth

					metaHeight += height
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

