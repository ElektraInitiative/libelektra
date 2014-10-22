import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.0
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1

ApplicationWindow {
    id: mainWindow

    visible: true
    width: Screen.desktopAvailableWidth
    height: Screen.desktopAvailableHeight

    title: "Elektra Editor"

    onClosing: {
        if(!undoManager.isClean()){
            close.accepted = false
            exitDialog.open()
        }
        else
            Qt.quit()
    }

    //**Properties********************************************************************************************//

    property int    deltaKeyAreaHeight: Math.round(keyArea.height - searchResultsArea.height*0.5 - defaultSpacing)
    property int    deltaKeyAreaWidth: Math.round(mainRow.width*0.7 - defaultSpacing)
    property int    deltaMetaAreaHeight: Math.round(metaArea.height - searchResultsArea.height*0.5)
    property var    keyAreaSelectedItem: null
    property var    metaAreaModel: (keyAreaSelectedItem === null ? null : keyAreaSelectedItem.metaValue)
    property int    pasteCounter: 0

    //Spacing & Margins recommended by KDE HIG
    property int    defaultSpacing: 4
    property int    defaultMargins: 8

    //**Signals & Slots****************************************************************************************//

    //Set up slots to catch signals from nodes/models
    Connections {
        target: externTreeModel

        onShowError: {
            showError(text, informativeText, detailedText)
        }
    }

    Connections {
        target: treeView.currentNode === null ? null : treeView.currentNode.node

        onShowError: {
            showError(text, informativeText, detailedText)
        }
    }

    Connections {
        target: (keyAreaSelectedItem === null || keyAreaSelectedItem === 'undefined') ? null : keyAreaSelectedItem.node

        onShowError: {
            showError(text, informativeText, detailedText)
        }
    }

    //**Functions**********************************************************************************************//

    //display an error message dialog
    function showError(text, informativeText, detailedText) {
        generalErrorDialog.text = text
        generalErrorDialog.informativeText = informativeText
        generalErrorDialog.detailedText = detailedText

        generalErrorDialog.open()
    }

    function cutKey() {
        console.log("cut Key")
        keyAreaView.copyPasteIndex = keyAreaView.currentRow
        keyAreaView.currentNodePath = treeView.currentNode.path

        undoManager.putToClipboard("cut", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.node, keyAreaSelectedItem.index)
        pasteCounter = 0
    }

    function cutBranch() {
        console.log("cut Branch")
        keyAreaView.copyPasteIndex = treeView.currentNode.index
        keyAreaView.currentNodePath = treeView.currentNode.path

        undoManager.putToClipboard("cut", treeView.currentNode.parentModel, treeView.currentNode.node, treeView.currentNode.index)
        pasteCounter = 0
    }

    function copyKey() {
        console.log("copy Key")
        keyAreaView.copyPasteIndex = keyAreaView.currentRow
        keyAreaView.currentNodePath = treeView.currentNode.path

        undoManager.putToClipboard("copy", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.node, keyAreaSelectedItem.index)
    }

    function copyBranch() {
        console.log("copy Branch")
    }

    function pasteKey() {
        console.log("paste Key")
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

    function pasteBranch() {
        console.log("paste Branch")
    }

    function deleteKey() {

        undoManager.createDeleteKeyCommand("deleteKey", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.node, keyAreaSelectedItem.index)

        metaAreaModel = null
        keyAreaSelectedItem = null
        //This is essential, without it, styleData.row of keyAreaView is not updated!!
        keyAreaView.model = null
        keyAreaView.model = treeView.currentNode.children

        if(keyAreaView.rowCount - 1 >= 0){
            keyAreaView.selection.clear()
            keyAreaView.selection.select(keyAreaView.currentRow)
            keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
        }
        else
            keyAreaSelectedItem = null
    }

    function deleteBranch() {

        undoManager.createDeleteKeyCommand("deleteBranch", treeView.currentNode.parentModel, treeView.currentNode.node, treeView.currentNode.index)
        treeView.currentNode = null
    }

    //**Colors*************************************************************************************************//

    //Get access to system colors
    SystemPalette {
        id: activePalette
        colorGroup: SystemPalette.Active
    }
    SystemPalette {
        id: inactivePalette
        colorGroup: SystemPalette.Inactive
    }
    SystemPalette {
        id: disabledPalette
        colorGroup: SystemPalette.Disabled
    }

    //**Windows************************************************************************************************//

    NewKeyWindow {
        id: newKeyWindow

        addButton.onClicked: {
            //add visual item
            qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
        }
    }

    EditKeyWindow {
        id: editKeyWindow

        addButton.onClicked: {
            //add visual item
            qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
        }
    }


    NewKeyWindow {
        id: newArrayWindow

        valueLabel.visible: false
        valueTextField.visible: false
        title: qsTr("Create new Array Entry")
        nameLabel.text: qsTr("Array Name: ")
        addButton.text: qsTr("New Array Entry")

        nameReadOnly: true
        valuePlaceHolder: "Array Value"
        isArray: true

        addButton.onClicked: {
            qmlMetaKeyModel.append({"metaName" : "#" + modelIndex, "metaValue" : ""})
        }
    }

    UnmountBackendWindow {
        id: unmountBackendWindow
        okButton.onClicked: unmountBackendWindow.close()
    }

    WizardLoader {
        id: wizardLoader
    }

    //**Dialogs************************************************************************************************//

    ExportDialog {
        id: exportDialog
    }

    ImportDialog {
        id: importDialog
    }

    MessageDialog {
        id: noNodeSelectedDialog

        title: qsTr("No Node selected")
        icon: StandardIcon.Critical
        text: qsTr("Please select a node to export the configuration below.")
    }

    MessageDialog {
        id:generalErrorDialog

        title: qsTr("Error")
        icon: StandardIcon.Critical
    }

    FileDialog {
        id: importFileDialog

        title: qsTr("Select File")
        onAccepted: importDialog.importTextField.text = importFileDialog.fileUrl.toString().replace("file://", "")
    }

    ExitDialog {
        id: exitDialog
    }

    //**Actions************************************************************************************************//

    Action {
        id:newKeyAction

        text: qsTr("Key...")
        iconSource: "icons/new-key.png"
        tooltip: qsTr("New Key")
        onTriggered: {
            if(treeView.currentItem === null){
                noNodeSelectedDialog.text = qsTr("Please select a node to create a new key.")
                noNodeSelectedDialog.open()
            }
            else{
                newKeyWindow.show()
            }
        }
    }

    Action {
        id:newArrayAction

        iconSource: "icons/new-array.png"
        text: qsTr("Array Entry...")
        onTriggered: {
            if(treeView.currentItem === null){
                noNodeSelectedDialog.text = qsTr("Please select a node to create a new array entry.")
                noNodeSelectedDialog.open()
            }
            else{
                newArrayWindow.show()
            }
        }
        enabled: false
    }

    Action {
        id:deleteAction

        text: qsTr("Delete")
        iconSource: "icons/delete.png"
        tooltip: "Delete"
        shortcut: StandardKey.Delete

        onTriggered: {
            if(treeView.currentNode !== null && keyAreaSelectedItem === null)
                deleteBranch()
            else
                deleteKey()
        }
    }

    Action {
        id: importAction

        text: qsTr("Import Configuration... ")
        iconSource: "icons/import.png"
        tooltip: qsTr("Import Configuration")

        onTriggered:{
            if(treeView.currentNode !== null){
                importDialog.show()
            }
            else{
                noNodeSelectedDialog.text = qsTr("Please select a node to import a configuration from file.")
                noNodeSelectedDialog.open()
            }
        }
    }

    Action {
        id: exportAction

        text: qsTr("Export Configuration... ")
        iconSource: "icons/export.png"
        tooltip: qsTr("Export Configuration")

        onTriggered: {
            if(treeView.currentNode !== null)
                exportDialog.open()
            else
                noNodeSelectedDialog.open()
        }
    }

    Action {
        id: undoAction

        text: qsTr("Undo")
        iconSource: "icons/undo.png"
        tooltip: qsTr("Undo")
        shortcut: StandardKey.Undo
        enabled: undoManager.canUndo
        onTriggered: {

            if(undoManager.undoText === "deleteKey"){
                undoManager.undo()

                var tmp = keyAreaView.model
                keyAreaView.model = null
                keyAreaView.model = tmp

//                if(keyAreaView.currentRow === -1)
//                    keyAreaView.currentRow = 0
//                else
//                    keyAreaView.__incrementCurrentIndex()

//                keyAreaView.selection.clear()
                //keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
            }
            else if(undoManager.undoText === "cut"){
                pasteCounter--
                undoManager.undo()
            }
            else{
                undoManager.undo()
            }
        }
    }

    Action {
        id: redoAction

        text: qsTr("Redo")
        iconSource: "icons/redo.png"
        tooltip: qsTr("Redo")
        shortcut: StandardKey.Redo
        enabled: undoManager.canRedo
        onTriggered: {

            if(undoManager.redoText === "deleteKey"){

                if(keyAreaView.currentRow > 0){
                    keyAreaView.__decrementCurrentIndex()
                }

                keyAreaView.selection.clear()
                metaAreaModel = null

                if(keyAreaView.rowCount > 0){
                    keyAreaView.selection.select(keyAreaView.currentRow)
                    keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
                }
                else
                    keyAreaSelectedItem = null
            }
            else if(undoManager.redoText === "deleteBranch"){

                if(metaAreaModel !== null)
                    metaAreaModel = null

                if(keyAreaSelectedItem !== null)
                    keyAreaSelectedItem = null
            }
            else if(undoManager.redoText === "cut"){
                pasteCounter--
            }

            undoManager.redo()
        }

    }

    Action {
        id: synchronizeAction

        text: qsTr("Synchronize")
        iconSource: "icons/synchronize.png"
        tooltip: qsTr("Synchronize")
        shortcut: StandardKey.Refresh
        onTriggered: {
            externTreeModel.synchronize()
            undoManager.setClean()
        }
    }

    Action {
        id: createBackendAction

        text: qsTr("Create Backend...")
        tooltip: qsTr("Create Backend")
        onTriggered: wizardLoader.show()
        enabled: false
    }

    Action {
        id: unmountBackendAction

        text: qsTr("Unmount Backend...")
        tooltip: qsTr("Unmount Backend")
        onTriggered: unmountBackendWindow.show()
    }

    Action {
        id: editAction

        iconSource: "icons/edit-rename.png"
        text: qsTr("Edit...")
        tooltip: qsTr("Edit")

        onTriggered: {
            editKeyWindow.show()
            editKeyWindow.populateMetaArea()
        }
    }

    Action {
        id: cutAction

        iconSource: "icons/edit-cut.png"
        text: qsTr("Cut")
        tooltip: qsTr("Cut")
        shortcut: StandardKey.Cut

        onTriggered: {
            if(treeView.currentNode !== null && keyAreaSelectedItem === null)
                cutBranch()
            else
                cutKey()
        }
    }

    Action {
        id: copyAction

        iconSource: "icons/edit-copy.png"
        text: qsTr("Copy")
        tooltip: qsTr("Copy")
        shortcut: StandardKey.Copy

        onTriggered: {
            if(treeView.currentNode !== null && keyAreaSelectedItem === null)
                copyBranch()
            else
                copyKey()
        }
    }

    Action {
        id: pasteAction

        iconSource: "icons/edit-paste.png"
        text: qsTr("Paste")
        tooltip: qsTr("Paste")
        shortcut: StandardKey.Paste

        onTriggered: {
            if(treeView.currentNode !== null && keyAreaSelectedItem === null)
                pasteBranch()
            else
                pasteKey()
        }
    }

    //**Menus & Toolbars***************************************************************************************//

    menuBar: MainMenuBar {
        id:mainMenuBar
    }

    toolBar: ToolBar {
        id:mainToolbar

        RowLayout {
            id:tbLayout

            anchors.fill: parent
            anchors.rightMargin: defaultSpacing

            ToolButton {
                id:tbNew
                action: newKeyAction
            }
            ToolButton {
                id:tbDelete
                action: deleteAction
            }
            ToolButton {
                id:tbImport
                action: importAction
            }
            ToolButton {
                id:tbExport
                action: exportAction
            }
            ToolButton {
                id:tbUndo
                action: undoAction
            }
            ToolButton {
                id:tbRedo
                action: redoAction
            }
            ToolButton {
                id:tbSynchronize
                action: synchronizeAction
            }
            Item {
                width: Math.round(mainWindow.width*0.3 - 7*tbRedo.width - 7*defaultSpacing - defaultMargins)
                height: tbNew.height
            }
            Image {
                id: searchLogo
                source: "icons/edit-find.png"
            }
            SearchField {
                id: searchField
                Layout.fillWidth: true
                focus: true
                onAccepted: {searchResultsListView.model = treeView.model.find(text); searchResultsListView.currentIndex = -1}
            }
        }
    }

    //TreeView Area Context Menu

    Menu {
        id: treeContextMenu

        Menu {
            id:tcmNew
            title: qsTr("New")

            MenuItem {
                id:tcmNewKey

                action: newKeyAction
            }
            MenuItem {
                id:tcmNewArray

                action: newArrayAction
            }
        }
        MenuItem {
            id: tcmEdit
            //TODO: if this node does not contain a key, its not possible to add metakeys
            //      if this node is renamed the changes are not permanent
            action: editAction
        }

        MenuSeparator{}

        MenuItem{
            id: tcmImport

            action: importAction
        }
        MenuItem{
            id: tcmExport

            action: exportAction
        }

        MenuSeparator{}

        MenuItem {
            id: tcmCut

            action: cutAction
        }
        MenuItem {
            id: tcmCopy

            action: copyAction

            //            onTriggered: {
            //                keyAreaView.copyPasteIndex = keyAreaView.currentRow
            //                keyAreaView.currentNodePath = treeView.currentNode.path

            //                undoManager.putToClipboard("copy", keyAreaSelectedItem.parentModel, keyAreaSelectedItem.node, keyAreaSelectedItem.index)
            //            }
        }
        MenuItem {
            id: tcmPaste

            action: pasteAction

            //            onTriggered: {
            //                keyAreaView.copyPasteIndex = -1
            //                keyAreaView.currentNodePath = ""

            //                if(undoManager.clipboardType === "copy"){
            //                    undoManager.createCopyKeyCommand(treeView.currentNode.node)
            //                }
            //                else if (undoManager.clipboardType === "cut"){

            //                    if(pasteCounter === 0){
            //                        undoManager.createCutKeyCommand(treeView.currentNode.node)
            //                        pasteCounter++
            //                    }
            //                    else{
            //                        undoManager.createCopyKeyCommand(treeView.currentNode.node)
            //                        pasteCounter++
            //                    }
            //                }
            //            }
        }
        MenuItem {
            id:tcmDelete

            action: deleteAction
        }
    }

    //Key Area Context Menu

    Menu {
        id: keyContextMenu

        MenuItem {
            id: kcmNewKey
            action: newKeyAction
        }
        MenuItem {
            id: kcmEdit
            action: editAction
        }
        MenuItem {
            id: kcmCut
            action: cutAction
        }
        MenuItem {
            id: kcmCopy
            action: copyAction
        }
        MenuItem {
            id: kcmPaste
            action: pasteAction
        }
        MenuItem {
            id: kcmDelete
            action: deleteAction
        }
    }

    //**Layouts & Views****************************************************************************************//

    Row {
        id: mainRow
        anchors.fill: parent
        anchors.margins: defaultMargins
        spacing: defaultSpacing

        BasicRectangle {
            id:treeViewRectangle

            width: Math.round(parent.width*0.3)
            height: parent.height

            TreeView {
                id: treeView
            }
        }
        Column {
            id: keyMetaColumn

            spacing: defaultSpacing

            BasicRectangle {
                id: keyArea

                width: deltaKeyAreaWidth
                height: Math.round(mainRow.height*0.7 - defaultSpacing)

                Component {
                    id: tableViewColumnDelegate

                    Item {
                        anchors.fill: parent
                        anchors.margins: defaultSpacing

                        Text{
                            anchors.verticalCenter: parent.verticalCenter
                            text: treeView.currentNode === null ? "" : styleData.value.replace(/\n/g, " ")
                            color: treeView.currentNode === null ? "transparent" : ((keyAreaView.copyPasteIndex === styleData.row && treeView.currentNode.path === keyAreaView.currentNodePath && keyAreaSelectedItem !== null) ? disabledPalette.text : activePalette.text)
                        }
                    }
                }

                TableView {
                    id: keyAreaView

                    property int copyPasteIndex
                    property string currentNodePath

                    anchors.fill: parent
                    anchors.margins: 2
                    frameVisible: false
                    alternatingRowColors: false
                    backgroundVisible: false
                    Component.onCompleted: currentRow = -1

                    model:{
                        if(treeView.currentNode !== null)
                            if(treeView.currentNode.childCount > 0 && treeView.currentNode.childrenHaveNoChildren){
                                treeView.currentNode.children
                            }
                    }

                    TableViewColumn {
                        id: nameColumn

                        role: "name"
                        title: qsTr("Name")
                        width: Math.round(keyArea.width*0.5)
                        delegate: tableViewColumnDelegate
                    }
                    TableViewColumn {
                        id: valueColumn

                        role: "value"
                        title: qsTr("Value")
                        width: Math.round(keyArea.width*0.5)
                        delegate: tableViewColumnDelegate
                    }

                    rowDelegate: Component {

                        Rectangle {
                            width: keyAreaView.width
                            color: styleData.selected ? activePalette.highlight : "transparent"

                            MouseArea {
                                anchors.fill: parent
                                acceptedButtons: Qt.LeftButton | Qt.RightButton

                                onClicked: {

                                    if(mouse.button === Qt.RightButton){
                                        keyContextMenu.popup()
                                    }
                                    else{
                                        keyAreaView.currentRow = styleData.row
                                        keyAreaSelectedItem = model.get(styleData.row)
                                        editKeyWindow.selectedNode = keyAreaSelectedItem
                                        metaAreaModel = keyAreaSelectedItem.metaValue

                                        keyAreaView.selection.clear()
                                        keyAreaView.selection.select(styleData.row)
                                    }
                                }

                                onDoubleClicked: {
                                    editKeyWindow.show()
                                    editKeyWindow.populateMetaArea()
                                }
                            }
                        }
                    }
                }
            }
            BasicRectangle {
                id: metaArea

                width: deltaKeyAreaWidth
                height: Math.round(mainRow.height*0.3)

                ScrollView {
                    id: metaAreaScrollView

                    anchors.fill: parent
                    anchors.margins: defaultMargins

                    ListView {
                        id: metaAreaListView

                        model: metaAreaModel

                        delegate: Text {
                            color: activePalette.text
                            text: keyAreaSelectedItem === null ? "" : (name + ": " + value)
                        }
                    }
                }
            }
            BasicRectangle {
                id: searchResultsArea

                width: deltaKeyAreaWidth
                height: Math.round(mainRow.height*0.2)
                visible: false

                Button {
                    id: searchResultsCloseButton

                    iconSource: "icons/dialog-close.png"
                    anchors.right: parent.right
                    anchors.top: parent.top
                    anchors.margins: Math.round(defaultMargins*0.25)
                    tooltip: qsTr("Close")
                    onClicked: keyMetaColumn.state = ""

                    style: ButtonStyle {
                        background: Rectangle {
                            color: searchResultsArea.color
                        }
                    }
                }

                ScrollView {
                    id: searchResultsScrollView

                    anchors.fill: parent
                    anchors.margins: defaultMargins
                    anchors.rightMargin: searchResultsCloseButton.width

                    ListView {
                        id: searchResultsListView

                        focus: false

                        highlight: Rectangle {
                            id: highlightBar
                            color: activePalette.highlight
                        }

                        delegate: Text {
                            color: activePalette.text
                            text: path

                            MouseArea {
                                anchors.fill: parent
                                onClicked: {searchResultsListView.currentIndex = index}
                            }
                        }
                    }
                }
            }
            states:
                State {
                name: "SHOW_SEARCH_RESULTS"

                PropertyChanges {
                    target: keyArea
                    height: deltaKeyAreaHeight
                }
                PropertyChanges {
                    target: metaArea
                    height: deltaMetaAreaHeight
                }
                PropertyChanges {
                    target: searchResultsArea
                    visible: true
                }
            }
        }
    }
    statusBar: StatusBar {
        id:mainStatusBar

        RowLayout {
            id: statusBarRow

            Label {
                id: path
                anchors.fill: parent
                anchors.leftMargin: defaultMargins
                text: treeView.currentNode === null ? "" : treeView.currentNode.path + "/" + (keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.name)
            }
        }
    }
}
