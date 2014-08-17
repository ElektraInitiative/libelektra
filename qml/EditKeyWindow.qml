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

        var metaData = {};

        //collect metadata in a map
        for(var i = 0; i < metaKeyModel.count; i++){
            metaData[metaKeyModel.get(i).metaName] = metaKeyModel.get(i).metaValue
        }

        console.log(isEdited)
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
        // metaAreaModel.get(keyAreaView.currentRow).node.setMeta(metaData)

        /*****************************************************************************************************************************/
        //            //delete metaKeys
        //            for(var i = 0; i < metaAreaModel.rowCount(); i++)
        //                metaAreaModel.get(i).node.deleteMeta(metaAreaModel.get(i).name)

        //            //clear old meta nodes
        //            metaAreaModel.clear()

        //            //insert new meta nodes
        //            for(var i = 0; i < metaKeyModel.count; i++)
        //                metaAreaModel.qmlInsertRow(i, keyAreaSelectedItem.node);

        //            //fill the meta nodes with provided names/values
        //            for(var i = 0; i < metaKeyModel.count; i++){
        //               metaAreaModel.setDataValue(i, [metaKeyModel.get(i).metaName, metaKeyModel.get(i).metaValue], "MetaValue")
        //            }

        metaKeyModel.clear()
    }
}
