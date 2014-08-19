import QtQuick 2.3

KeyWindow {

    title: qsTr("Edit Key")

    path: treeView.currentNode === null ? "" : treeView.currentNode.path
    keyName: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.name
    keyValue: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.value

    function populateMetaArea() {
        for(var i = 0; i < metaAreaModel.rowCount(); i++){
            qmlMetaKeyModel.append({"metaName" : metaAreaListView.model.get(i).name, "metaValue" : metaAreaListView.model.get(i).value})
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
            undoManager.createEditCommand(keyAreaView.model, keyAreaView.currentRow, keyName.toString(), keyValue.toString(), keyAreaSelectedItem.metaValue,
                                          nameTextField.text, valueTextField.text, metaData)

        //set key name & value
        keyAreaView.model.setData(keyAreaView.currentRow, nameTextField.text, "Name")
        keyAreaView.model.setData(keyAreaView.currentRow, valueTextField.text, "Value")


        console.log(keyAreaView.currentRow)

        //set metaData
        keyAreaSelectedItem.node.setMeta(metaData)

        qmlMetaKeyModel.clear()
    }
}
