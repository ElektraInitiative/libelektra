import QtQuick 2.0
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

Window {

    id: basicWindow

    property alias okButton: okButton
    property alias cancelButton: cancelButton
    default property alias contents: placeholder.children

    minimumWidth: Math.round(mainWindow.width*0.35)
    minimumHeight: Math.round(mainWindow.height*0.6)

    x: Math.round(Screen.desktopAvailableWidth*0.5-width*0.5)
    y: Math.round(Screen.desktopAvailableHeight*0.5-height*0.5)

    color: activePalette.window

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: defaultMargins
        spacing: defaultSpacing

        Item {
            id: placeholder
            Layout.fillHeight: true
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
        }

        RowLayout {
            anchors.bottom: parent.bottom
            anchors.right: parent.right

            Button {
                id:okButton
                text: "Ok"
            }
            Button {
                id:cancelButton
                text: "Cancel"
                isDefault: true
                onClicked: basicWindow.visible = false
            }
        }
    }
}
