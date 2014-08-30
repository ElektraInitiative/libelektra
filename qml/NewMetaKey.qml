import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Layouts 1.1
import QtQuick.Controls.Styles 1.1

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
            text: metaName
        }
        TextField {
            id: metaValueField
            Layout.fillWidth: true
            placeholderText : qsTr("Meta Key Value...")
            text: metaValue
        }
        Button {
            id:deleteMetaButton
            iconSource: "icons/application-exit.png"

            onClicked: {
                qmlMetaKeyModel.remove(index)// remove the visual item
                isEdited = true
            }
        }
    }
}


