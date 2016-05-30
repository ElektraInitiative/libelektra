import QtQuick 2.2
import QtQuick.Controls 1.1
import "HelperFunctions.js" as Helper

TextField {
	id: searchField

	placeholderText: qsTr("Find Key ...")

	onAccepted: {
		if (searchField.state == "hasText")
			keyMetaColumn.state = "SHOW_SEARCH_RESULTS"
	}

	Image {
		id: clear
		anchors {
			right: parent.right
			rightMargin: defaultMargins
			verticalCenter: parent.verticalCenter
		}
		source: Helper.useIconSource("edit-clear")
		sourceSize.width: 16
		sourceSize.height: 16
		opacity: 0

		MouseArea {
			anchors.fill: parent
			onClicked: {
				searchField.text = ""
			}
		}
	}
	states: State {
		name: "hasText"
		when: searchField.text !== ""

		PropertyChanges {
			target: clear
			opacity: 1
		}
	}
}
