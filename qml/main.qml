import QtQuick 2.3
import QtQuick.Controls 1.2
import QtQuick.Window 2.0
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.2

ApplicationWindow {
    id: mainWindow

    visible: true
    width: Screen.desktopAvailableWidth
    height: Screen.desktopAvailableHeight

    title: "Elektra Editor"

    property int deltaKeyAreaHeight: Math.round(keyArea.height - searchResultsArea.height*0.5 - defaultSpacing)
    property int deltaKeyAreaWidth: Math.round(mainRow.width*0.7 - defaultSpacing)
    property int deltaMetaAreaHeight: Math.round(metaArea.height - searchResultsArea.height*0.5)
    property var keyAreaSelectedItem: null
    //TreeViewModel
    property var metaAreaModel: (keyAreaSelectedItem === null ? null : keyAreaSelectedItem.metaValue)

    //Spacing & Margins recommended by KDE HIG
    property int defaultSpacing: 4
    property int defaultMargins: 8

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

    NewKeyWindow {
        id: newKeyWindow
        title: qsTr("Create new Key")
    }

    NewKeyWindow {
        id: editKeyWindow
        title: qsTr("Edit Key")
        path: treeView.currentNode === null ? "" : treeView.currentNode.path
        keyName: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.name
        keyValue: keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.value
        metaCount: metaAreaModel === null ? 0 : metaAreaModel.qmlRowCount()
    }

    NewKeyWindow {
        id: newArrayWindow
        title: qsTr("Create new Array Entry")
        valueLayout.visible: false
        nameLabel.text: qsTr("Array Name: ")
        addButton.text: qsTr("New Array Entry")
    }

    UnmountBackendWindow {
        id: unmountBackendWindow
    }

    WizardLoader {
        id: wizardLoader
    }

    FileDialog {
        id: exportDialog
        title: qsTr("Export to file")
        selectExisting: false
    }

    Action {
        id:newKeyAction
        text: qsTr("Key...")
        iconSource: "icons/new.png"
        tooltip: qsTr("New Key")
        onTriggered: newKeyWindow.show()
    }

    Action {
        id:newArrayAction
        text: qsTr("Array Entry...")
        onTriggered: newArrayWindow.show()
    }

    Action {
        id:deleteAction
        text: qsTr("Delete")
        iconSource: "icons/delete.png"
        tooltip: "Delete"
        shortcut: StandardKey.Delete
    }

    Action {
        id: importAction
        text: qsTr("Import Configuration... ")
        iconSource: "icons/import.png"
        tooltip: qsTr("Import Configuration")
        onTriggered: importDialog.open()
    }

    Action {
        id: exportAction
        text: qsTr("Export Configuration... ")
        iconSource: "icons/export.png"
        tooltip: qsTr("Export Configuration")
        onTriggered: exportDialog.open()
    }

    Action {
        id: undoAction
        text: qsTr("Undo")
        iconSource: "icons/undo.png"
        tooltip: qsTr("Undo")
        shortcut: StandardKey.Undo
        enabled: false
    }

    Action {
        id: redoAction
        text: qsTr("Redo")
        iconSource: "icons/redo.png"
        tooltip: qsTr("Redo")
        shortcut: StandardKey.Redo
        enabled: false
    }

    Action {
        id: synchronizeAction
        text: qsTr("Synchronize")
        iconSource: "icons/synchronize.png"
        tooltip: qsTr("Synchronize")
        shortcut: StandardKey.Refresh
        onTriggered: externTreeModel.synchronize()
        enabled: false
    }

    Action {
        id: createBackendAction
        text: qsTr("Create Backend...")
        tooltip: qsTr("Create Backend")
        onTriggered: wizardLoader.show()
    }

    Action {
        id: unmountBackendAction
        text: qsTr("Unmount Backend...")
        tooltip: qsTr("Unmount Backend")
        onTriggered: unmountBackendWindow.show()
    }

    Action {
        id: editAction
        text: qsTr("Edit...")
        tooltip: qsTr("Edit")
    }

    Action {
        id: cutAction
        text: qsTr("Cut")
        tooltip: qsTr("Cut")
        shortcut: StandardKey.Cut
        enabled: false
    }

    Action {
        id: copyAction
        text: qsTr("Copy")
        tooltip: qsTr("Copy")
        shortcut: StandardKey.Copy
        enabled: false
    }

    Action {
        id: pasteAction
        text: qsTr("Paste")
        tooltip: qsTr("Paste")
        shortcut: StandardKey.Paste
        enabled: false
    }

    menuBar: MenuBar {
        id:mainMenuBar

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
                title: "New"

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
            id: kcmDelete
            action: deleteAction
            onTriggered: {
                keyAreaView.model.removeRow(keyAreaView.tableIndex)
                keyAreaView.__decrementCurrentIndex()
                keyAreaSelectedItem = keyAreaView.model.get(keyAreaView.currentRow)
            }
        }
        MenuItem {
            id: kcmEdit
            action: editAction
            onTriggered: {
                editKeyWindow.show()
                editKeyWindow.populateMetaArea()
            }
        }
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
                height: 28
            }

            SearchField {
                id: searchField
                Layout.fillWidth: true
                focus: true
                onAccepted: {searchResultsListView.model = treeView.model.find(text); searchResultsListView.currentIndex = -1}
            }
        }
    }

    ListModel {
        id: mountedBackendsModel
        ListElement {
            backendName: "Test"
        }
        ListElement {
            backendName: "Backend1"
        }
        ListElement {
            backendName: "Backend2"
        }
    }

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

                TableView {
                    id: keyAreaView

                    anchors.fill: parent
                    anchors.margins: 2
                    frameVisible: false
                    alternatingRowColors: false
                    backgroundVisible: false
                    Component.onCompleted: currentRow = -1
                    //QVariantMap
                    onClicked: keyAreaSelectedItem = model.get(currentRow)

                    model:{
                        if(treeView.currentNode !== null)
                            if(treeView.currentNode.childCount > 0 && treeView.currentNode.childrenHaveNoChildren)
                                //TreeViewModel
                                treeView.currentNode.children
                    }
                    TableViewColumn {
                        role: "name"
                        title: qsTr("Name")
                        width: Math.round(keyArea.width*0.5)
                    }
                    TableViewColumn {
                        role: "value"
                        title: qsTr("Value")
                        width: Math.round(keyArea.width*0.5)
                    }
                    rowDelegate: Rectangle {
                        width: keyAreaView.width
                        color: styleData.selected ? activePalette.highlight : "transparent"
                        MouseArea {
                            propagateComposedEvents: true
                            anchors.fill: parent
                            acceptedButtons: Qt.RightButton
                            onClicked: keyContextMenu.popup()
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
                text: treeView.currentNode === null ? "" : treeView.currentNode.path + "/" + (keyAreaSelectedItem === null ? "" : keyAreaSelectedItem.name)
            }
        }
    }
}
