import QtQuick 2.3

KeyWindow {

    title: qsTr("Create new Key")
    path: treeView.currentNode === null ? "" : treeView.currentNode.path

    function editAccepted() {

        var metaData = {};

        //collect metadata
        for(var i = 0; i < qmlMetaKeyModel.count; i++)
            metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue

        //create UndoCommand
        undoManager.createNewKeyCommand(externTreeModel, treeView.currentNode.path + "/" + nameTextField.text, valueTextField.text, metaData)

//        //insert new node
//        externTreeModel.createNewNode(treeView.currentNode.path + "/" + nameTextField.text, valueTextField.text, metaData)

        nameTextField.text = ""
        valueTextField.text = ""
        nameTextField.focus = true
        qmlMetaKeyModel.clear()
    }
}
