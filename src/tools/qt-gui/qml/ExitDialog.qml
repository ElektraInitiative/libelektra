import QtQuick 2.2
import QtQuick.Dialogs 1.1

MessageDialog {
    icon: StandardIcon.Warning
    title: qsTr("Close Elektra Editor")
    text: qsTr("The configuration has been modified.")
    informativeText: "Do you want to save your changes or discard them?"
    standardButtons: StandardButton.Save | StandardButton.Discard | StandardButton.Cancel
    onAccepted: {
        externTreeModel.synchronize()
        Qt.quit()
    }
    onDiscard: Qt.quit()
}
