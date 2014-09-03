import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

Item {
    id: template

    property string usedNames
    property alias wizardText: wizardText
    property alias buttonRow: buttonRow
    property alias label: label
    property alias textField: textField

    ColumnLayout {
        id: wizardlayout

        anchors.fill: parent
        anchors.margins: defaultMargins
        spacing: defaultMargins

        Text {
            id: wizardText

            wrapMode: Text.WordWrap
            color: activePalette.text
            //height: Math.round(wizardLoader.height*0.7)
            Layout.fillHeight: true
            Layout.fillWidth: true
            Layout.alignment:  Qt.AlignVCenter
            text: ""
        }
        RowLayout {
            spacing: defaultSpacing

            Label {
                id:label
                text: ""
            }
            TextField {
                id:textField
                Layout.fillWidth: true
            }
        }
        ButtonRow {
            id: buttonRow
            cancelButton.onClicked: wizardLoader.close()
        }
    }
}
