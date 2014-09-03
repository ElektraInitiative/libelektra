import QtQuick 2.2
import QtQuick.Dialogs 1.1

MessageDialog {
    icon: StandardIcon.Warning
    text: qsTr("The configuration has been modified.\nDo you want to save your changes?")
    standardButtons: StandardButton.Save | StandardButton.Discard | StandardButton.Cancel
    onAccepted: {
        externTreeModel.synchronize()
        Qt.quit()
    }
    onDiscard: Qt.quit()
}
