import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1

BasicWindow {

	title: qsTr("About Elektra Editor")

	width: Math.ceil(mainWindow.width*0.25)
	height: Math.ceil(mainWindow.width*0.25)

	ColumnLayout {
		anchors.fill: parent
		spacing: defaultMargins

		RowLayout {
			spacing: 2*defaultMargins
			Image {
				source: "icons/elektra-logo.png"
			}
			Column {
				Text {
					text: "Elektra Editor"
					font.bold: true
					color: activePalette.text
				}
				Text {
					text: "Version: 0.0.3 (alpha)"
					color: activePalette.text
				}
			}
		}

		RowLayout {
			TabView {
				id: tabs

				anchors.fill: parent
				Layout.fillHeight: true
				Layout.fillWidth: true

				Tab {
					id: aboutTab

					title: qsTr("About")

					TextArea{
						property string link: "http://www.libelektra.org"
						readOnly: true
						textFormat: TextEdit.RichText
						text: "<html><style type=\"text/css\"></style><b>Elektra provides a universal and secure framework to store configuration " +
						"parameters in a global, hierarchical key database.</b><br><br><br>For further information and for reporting bugs " +
						"<a href=\"" + link + "\">visit Elektra's Website</a></html>"
						onLinkActivated: Qt.openUrlExternally(link)
					}
				}
				Tab {
					id: authorsTab

					title: qsTr("Authors")
					TextArea {
						property string pancheri: "mailto:e0003088@student.tuwien.ac.at"
						property string loose: "mailto:christian.loose@hamburg.de"
						property string jens: "http://qt-project.org/forums/viewthread/30521/#146845"
						readOnly: true
						textFormat: TextEdit.RichText
						textMargin: defaultSpacing
						wrapMode: Text.WordWrap
						text: "<html>" +
							  "<style type=\"text/css\"></style>" +
							  "<p>Elektra Editor created by <a href=\"" + pancheri + "\">Raffael Pancheri</a></p>" +
							  "<p>DiscountMarkdownConverter used with kind permission by <a href=\"" + loose + "\">Christian Loose</a></p>" +
							  "<p>TreeView based on code <a href=\"" + jens + "\">posted by Jens</a></p>" +
							  "</html>"
						onLinkActivated: {
							Qt.openUrlExternally(link)
						}
					}
				}
			}
		}
	}

	okButton.visible: false
	cancelButton.text: qsTr("&Close")
	cancelButton.action.onTriggered: aboutWindow.close()
}

