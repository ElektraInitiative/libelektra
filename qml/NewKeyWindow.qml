import QtQuick 2.0

KeyWindow {

    title: qsTr("Create new Key")
    path: treeView.currentNode === null ? "" : treeView.currentNode.path
    isEdited: false

    function editAccepted() {

        var metaData = {};

        //collect metadata
        for(var i = 0; i < metaKeyModel.count; i++)
            metaData[metaKeyModel.get(i).metaName] = metaKeyModel.get(i).metaValue


        //insert new node
        externTreeModel.createNewNode(treeView.currentNode.path + "/" + nameTextField.text, valueTextField.text, metaData)

        nameTextField.text = ""
        valueTextField.text = ""
        nameTextField.focus = true
        metaKeyModel.clear()
        //            externTreeModel.synchronize()
    }
}
