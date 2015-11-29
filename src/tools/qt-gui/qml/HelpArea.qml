import QtQuick 2.2
import "TooltipCreator.js" as TooltipCreator

MouseArea {
	property string helpText
	property var	helpParent: parent

	anchors.fill: parent
	cursorShape: helpMode ? Qt.WhatsThisCursor : Qt.ArrowCursor
	propagateComposedEvents: true
	hoverEnabled: helpMode

	onPressed: {
		if (helpMode){
			helpMode = false
			TooltipCreator.destroy()
		}

		mouse.accepted = false
	}

	onHoveredChanged: {
		if (helpMode && containsMouse)
			TooltipCreator.createHelp(helpText, defaultMargins, helpParent.mapToItem(null, 2*defaultMargins, 0).x, helpParent.mapToItem(null, 0, 2*defaultMargins).y, mainWindow)
	}

	onExited: TooltipCreator.destroy()
}

