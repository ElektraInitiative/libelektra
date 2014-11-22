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

	width: Math.max(maxWidth + 2*defaultMargins, keyText.paintedWidth + 2*defaultMargins)
	height: (metaView.height > 0) ? (metaView.height +  keyText.paintedHeight + 3*defaultMargins) : (metaView.height +  keyText.paintedHeight + 2*defaultMargins)
	color: inActivePalette.base

	ColumnLayout {
		id: layout

		anchors.fill: parent
		anchors.centerIn: parent
		anchors.margins: defaultMargins
		spacing: defaultMargins

		Row {
			Label {
				id: keyText
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
			Label {
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

