import QtQuick 2.3

KeyWindow {

    title: qsTr("Edit Key")

    path: treeView.currentNode === null ? "" : treeView.currentNode.path
    keyName: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.name
    keyValue: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.value

    function populateMetaArea() {
        for(var i = 0; i < metaAreaModel.rowCount(); i++){
            metaKeyModel.append({"metaName" : metaAreaListView.model.get(i).name, "metaValue" : metaAreaListView.model.get(i).value})
        }
    }

    function editAccepted() {

        //TODO: check if user has edited the key
        if(keyName !== nameTextField.text || keyValue !== valueTextField.text)
            isEdited = true;

        var metaData = {};

        //collect metadata in a map
        for(var i = 0; i < metaKeyModel.count; i++){
            metaData[metaKeyModel.get(i).metaName] = metaKeyModel.get(i).metaValue
        }

        //create undo command
        if(isEdited)
            undoManager.createEditCommand(keyAreaView.model, keyAreaView.currentRow, keyName.toString(), keyValue.toString(), keyAreaSelectedItem.metaValue,
                                          nameTextField.text, valueTextField.text, metaData)

        //set key name & value
        keyAreaView.model.setDataValue(keyAreaView.currentRow, nameTextField.text, "Name")
        keyAreaView.model.setDataValue(keyAreaView.currentRow, valueTextField.text, "Value")


        console.log(keyAreaView.currentRow)

        //set metaData
        keyAreaSelectedItem.node.setMeta(metaData)

        metaKeyModel.clear()
    }
}
