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
                var idx = index // index of this item
                metaKeyModel.remove(idx)// remove the visual item
                metaAreaListView.model.get(idx).node.deleteMeta(metaNameField.text)// delete the metakey in the node in the metaArea model
                metaAreaListView.model.removeRow(idx)// remove the node from the metaArea model
            }
        }
    }
}


