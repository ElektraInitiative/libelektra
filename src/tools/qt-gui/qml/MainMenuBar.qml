import QtQuick 2.2
import QtQuick.Controls 1.1

MenuBar {

    Menu {
        id:dbdatabase
        title: qsTr("&Database")

        MenuItem {
            id:dbImport
            action: importAction
        }
        MenuItem {
            id:dbExport
            action: exportAction
        }

        MenuSeparator{}

        MenuItem {
            id:dbCreateBackend
            action: createBackendAction
        }
        MenuItem {
            id:dbUnmountBackend
            action: unmountBackendAction
        }

        MenuSeparator{}

        MenuItem {
            id:dbExit
            text: qsTr("Exit")
            shortcut: StandardKey.Quit
            onTriggered: {
                if(!undoManager.isClean())
                    exitDialog.open()
                else
                    Qt.quit()
            }
        }
    }

    Menu {
        id:edit
        title: qsTr("&Edit")

        MenuItem {
            id:edUndo
            action: undoAction
        }
        MenuItem {
            id:edRedo
            action: redoAction
        }

        MenuSeparator{}

        Menu {
            id:edNew
            title: qsTr("New")

            MenuItem {
                id:edNewKey
                action: newKeyAction
            }
            MenuItem {
                id:edNewArray
                action: newArrayAction
            }
        }

        MenuItem {
            id:edEdit
            action: editAction
        }

        MenuSeparator{}

        MenuItem {
            id:edCut
            action: cutAction
        }
        MenuItem {
            id:edCopy
            action: copyAction
        }
        MenuItem {
            id:edPaste
            action: pasteAction
        }
        MenuItem {
            id:edDelete
            action: deleteAction
        }
    }

    Menu {
        id:about
        title: qsTr("&About")
        MenuItem {
            text: qsTr("Credits ")
        }
    }
}


