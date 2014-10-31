import QtQuick 2.2

KeyWindow {

    title: qsTr("Create new Key")
    path: treeView.currentNode === null ? "" : treeView.currentNode.path

    function editAccepted() {

        var metaData = {};

        //collect metadata
        for(var i = 0; i < qmlMetaKeyModel.count; i++)
            metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

        //create UndoCommand
        undoManager.createNewKeyCommand(treeView.currentNode.node, nameTextField.text, valueTextField.text, metaData)

        nameTextField.text = ""
        valueTextField.text = ""
        nameTextField.focus = true
        qmlMetaKeyModel.clear()
        externTreeModel.refresh()
    }
}
