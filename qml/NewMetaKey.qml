import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.2

Item {
    id: metaInfoItem
    width: metaArea.width - 3*defaultMargins
    height: metaNameField.height

    property alias metaNameField: metaNameField
    property alias metaValueField: metaValueField

    RowLayout {
        anchors.fill: parent
        anchors.margins: defaultMargins

        TextField {
            id: metaNameField
            Layout.fillWidth: true
            placeholderText : qsTr("Meta Key Name...")
        }
        TextField {
            id: metaValueField
            Layout.fillWidth: true
            placeholderText : qsTr("Meta Key Value...")
        }
        Button {
            id:deleteMetaButton
            iconSource: "icons/application-exit.png"
        }
    }
}


