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

    Menu {
        id: treeContextMenu
        //MenuItem {
        //id:tcmDelete
        //action: deleteAction
        //}
    }

    Menu {
        id: keyContextMenu

        MenuItem {
            id: kcmNewKey
            action: newKeyAction
        }
        MenuItem {
            id: kcmEdit
            action: editAction
            onTriggered: {
                editKeyWindow.show()
                editKeyWindow.populateMetaArea()
            }
        }
        MenuItem {
            id: kcmCut

            action: cutAction
            onTriggered: {
                console.log("cut")
                keyAreaView.copyPasteIndex = keyAreaView.currentRow
                keyAreaView.currentNodePath = treeView.currentNode.path

                undoManager.putToClipboard("cut", keyAreaView.model, keyAreaSelectedItem.node, keyAreaView.currentRow)
                pasteCounter = 0
            }
        }
        MenuItem {
            id: kcmCopy

            action: copyAction

            onTriggered: {
                console.log("copy")
                keyAreaView.copyPasteIndex = keyAreaView.currentRow
                keyAreaView.currentNodePath = treeView.currentNode.path

                undoManager.putToClipboard("copy", keyAreaView.model, keyAreaSelectedItem.node, keyAreaView.currentRow)
            }
        }
        MenuItem {
            id: kcmPaste

            action: pasteAction
            onTriggered: {
                console.log("paste")
                keyAreaView.copyPasteIndex = -1
                keyAreaView.currentNodePath = ""

                if(undoManager.clipboardType === "copy"){
                    undoManager.createCopyKeyCommand(treeView.currentNode.node)
                }
                else if (undoManager.clipboardType === "cut"){

                    if(pasteCounter === 0){
                        undoManager.createCutKeyCommand(treeView.currentNode.node)
                        pasteCounter++
                    }
                    else{
                        undoManager.createCopyKeyCommand(treeView.currentNode.node)
                        pasteCounter++
                    }
                }
            }
        }
        MenuItem {
            id: kcmDelete

            action: deleteAction

            onTriggered: {

                if(keyAreaSelectedItem !== null){
                    undoManager.createDeleteKeyCommand("deleteKey", keyAreaView.model, keyAreaSelectedItem.node, keyAreaView.currentRow)

                    keyAreaView.__decrementCurrentIndex()
                    keyAreaView.selection.clear()
                    keyAreaView.selection.select(keyAreaView.currentRow)
                    keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)

                    if(keyAreaView.rowCount !== 0)
                        keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
                    else
                        keyAreaSelectedItem = null
                }
                else if(treeView.currentNode !== null && keyAreaSelectedItem === null){
                    undoManager.createDeleteKeyCommand("deleteBranch", treeView.currentNode.parentModel, treeView.currentNode.node, treeView.currentNode.index)
                }
            }
        }
    }
}


