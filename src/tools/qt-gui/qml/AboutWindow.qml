import QtQuick 2.2
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.1
import "HelperFunctions.js" as Helper

BasicWindow {

	title: qsTr("About Elektra Qt Editor")

	width: Math.ceil(mainWindow.width*0.25)
	height: Math.ceil(mainWindow.width*0.25)

	ColumnLayout {
		anchors.fill: parent
		spacing: defaultMargins

		RowLayout {
			spacing: 2*defaultMargins
			Image {
				source: Helper.useIconSource("elektra")
			}
			Column {
				Text {
					text: "Elektra Qt Editor"
					font.bold: true
					color: activePalette.windowText
				}
				Text {
					text: "Version: " + version
					color: activePalette.windowText
				}
			}
		}

		RowLayout {
			TabView {
				id: tabs

				//anchors.fill: parent
				Layout.fillHeight: true
				Layout.fillWidth: true

				Tab {
					id: aboutTab

					title: qsTr("About")

					TextArea{
						property string link: "https://www.libelektra.org"
						readOnly: true
						textFormat: TextEdit.RichText
						backgroundVisible: false
						textMargin: defaultMargins
						frameVisible: false
						text: "<html><style type=\"text/css\"></style><b>Elektra provides a universal and secure framework to store configuration " +
						"parameters in a global, hierarchical key database.</b><br><br><br>For further information and for reporting bugs " +
						"<a href=\"" + link + "\">visit Elektra’s Website</a></html>"
						onLinkActivated: Qt.openUrlExternally(link)
					}
				}
				Tab {
					id: authorsTab

					title: qsTr("Authors")
					TextArea {
                        property string pancheri: "mailto:raffael@libelektra.org"
						property string loose: "mailto:christian.loose@hamburg.de"
						property string jens: "http://qt-project.org/forums/viewthread/30521/#146845"
						property string ian: "mailto:easyxtarget@gmail.com"
						property string gabriel: "mailto:rauter.gabriel@gmail.com"
						readOnly: true
						textFormat: TextEdit.RichText
						textMargin: defaultMargins
						wrapMode: Text.WordWrap
						backgroundVisible: false
						frameVisible: false
						text: "<html>" +
							  "<style type=\"text/css\"></style>" +
							  "<p>Elektra Qt Editor designed and implemented by <a href=\"" + pancheri + "\">Raffael Pancheri</a></p>" +
							  "<p>DiscountMarkdownConverter used with kind permission by <a href=\"" + loose + "\">Christian Loose</a></p>" +
							  "<p>TreeView based on code <a href=\"" + jens + "\">posted by Jens</a></p>" +
							  "<p>Helptext by <a href=\"" + ian + "\">Ian Donelly</a></p>" +
							  "<p>XDG Icon Theme support by <a href=\"" + gabriel + "\">Gabriel Rauter</a></p>" +
							  "</html>"
						onLinkActivated: {
							Qt.openUrlExternally(link)
						}
					}
				}
				Tab {
					id: licenseTab

					title: qsTr("License")
					TextArea {
						property string ccUrl: "https://creativecommons.org/licenses/by-sa/3.0/legalcode"
						property string oxygenUrl: "http://www.oxygen-icons.org/"
						readOnly: true
						textFormat: TextEdit.RichText
						textMargin: defaultMargins
						wrapMode: Text.WordWrap
						backgroundVisible: false
						frameVisible: false
						text: "<html>" +
							  "<style type=\"text/css\"></style>" +
							  "<p><a href=\"" + oxygenUrl + "\">Oxygen Icons</a> are used under <a href=\"" + ccUrl + "\">Creative Commons BY-SA 3.0</a></p></html>"
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

