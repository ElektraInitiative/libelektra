import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

RowLayout {
    id: buttonRow
    anchors.bottom: parent.bottom
    anchors.right: parent.right

    property alias backButton: backButton
    property alias nextButton: nextButton
    property alias finishButton: finishButton
    property alias cancelButton: cancelButton

    Button {
        id:backButton
        text: qsTr("&Back")
        iconSource: "icons/go-previous.png"
    }
    Button {
        id:nextButton
        text: qsTr("&Next")
        iconSource: "icons/go-next.png"
    }
    Button {
        id:finishButton
        text: qsTr("&Finish")
        iconSource: "icons/dialog-ok.png"
    }
    Button {
        id:cancelButton
        text: qsTr("&Cancel")
        iconSource: "icons/dialog-cancel.png"
    }
}
