import QtQuick 2.2

KeyWindow {

    title: qsTr("Edit Key")

    property var selectedNode: null

    path: treeView.currentNode === null ? "" : treeView.currentNode.path
    keyName: selectedNode === null ? "" : selectedNode.name
    keyValue: selectedNode === null ? "" : selectedNode.value

    function populateMetaArea() {
        console.log(selectedNode.metaValue.rowCount())
        for(var i = 0; i < selectedNode.metaValue.rowCount(); i++){
            qmlMetaKeyModel.append({"metaName" : selectedNode.metaValue.get(i).name, "metaValue" : selectedNode.metaValue.get(i).value})
        }
    }

    function editAccepted() {

        var metaData = {};

        //collect metadata in a map
        for(var i = 0; i < qmlMetaKeyModel.count; i++){
            metaData[qmlMetaKeyModel.get(i).metaName] = qmlMetaKeyModel.get(i).metaValue
        }

        //create undo command
        if(isEdited)
            undoManager.createEditKeyCommand(selectedNode.parentModel, selectedNode.index, keyName.toString(), keyValue.toString(), selectedNode.metaValue,
                                          nameTextField.text, valueTextField.text, metaData)

        qmlMetaKeyModel.clear()
    }
}
