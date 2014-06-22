import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.1
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1
import Qt.labs.folderlistmodel 2.0

ApplicationWindow {
    id: mainWindow

    visible: true
    width: Screen.desktopAvailableWidth
    height: Screen.desktopAvailableHeight

    title: "Elektra Editor"

    property int deltaKeyAreaHeight: Math.round(keyArea.height-searchResultsArea.height*0.5-defaultSpacing)
    property int deltaKeyAreaWidth: Math.round(mainRow.width*0.7-defaultSpacing)
    property int deltaMetaAreaHeight: Math.round(metaArea.height-searchResultsArea.height*0.5)

    //Spacing & Margins recommended by KDE HIG
    property int defaultSpacing: 4
    property int defaultMargins: 8

    //    DefaultFileDialog {
    //        id: importDialog
    //    }

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
        enabled: false
        //onTriggered:
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
        //        onTriggered: importDialog.open()
    }

    Action {
        id: redoAction
        text: qsTr("Redo")
        iconSource: "icons/redo.png"
        tooltip: qsTr("Redo")
        shortcut: StandardKey.Redo
        enabled: false
        //        onTriggered: importDialog.open()
    }

    Action {
        id: synchronizeAction
        text: qsTr("Synchronize")
        iconSource: "icons/synchronize.png"
        tooltip: qsTr("Synchronize")
        shortcut: StandardKey.Refresh
        onTriggered: externTreeModel.synchronize
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
        enabled: false
        //        onTriggered:
    }

    Action {
        id: cutAction
        text: qsTr("Cut")
        tooltip: qsTr("Cut")
        shortcut: StandardKey.Cut
        enabled: false
        //        onTriggered:
    }

    Action {
        id: copyAction
        text: qsTr("Copy")
        tooltip: qsTr("Copy")
        shortcut: StandardKey.Copy
        enabled: false
        //        onTriggered:
    }

    Action {
        id: pasteAction
        text: qsTr("Paste")
        tooltip: qsTr("Paste")
        shortcut: StandardKey.Paste
        enabled: false
        //        onTriggered:
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

    toolBar: ToolBar {
        id:mainToolbar

        RowLayout {
            id:tbLayout

            anchors.fill: parent
            //            anchors.leftMargin: defaultMargins
            anchors.rightMargin: defaultMargins

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
            }
        }
    }

    ListModel {
        id: keyModel

        ListElement {
            keyName: "Key 1"
            keyValue: "Value 1"
        }
        ListElement {
            keyName: "Key 2"
            keyValue: "Value 2"
        }
        ListElement {
            keyName: "Key 3"
            keyValue: "Value 3"
        }
    }

    ListModel {
        id: metaInfoModel
        ListElement {
            keyName: "Keyname"
            keyValue: "Key1"
        }
        ListElement {
            keyName: "Keytype"
            keyValue: "String"
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

            ScrollView {
                id: treeView

                anchors.fill: parent
                anchors.margins: defaultSpacing

                property var model: externTreeModel
                property int rowHeight: 19
                property int columnIndent: 22
                property var currentNode
                property var currentItem

                property Component delegate: Label {
                    id: label
                    text: model.modelData.name
                    color: activePalette.windowText
//                    Component.onCompleted: console.log("modelData" + model.modelData + ", model: " + model + ", name: " + model.name + ", path: " + model.path + ", elements: " + model.children + ", childCount: " + model.childCount)
                }
                contentItem: Loader {
                    id: content

                    onLoaded: item.isRoot = true
                    sourceComponent: treeBranch
                    property var elements: treeView.model

                    Column {
                        anchors.fill: parent
                        Repeater {
                            model: 1 + Math.max(treeView.contentItem.height, treeView.height) / treeView.rowHeight
                            Rectangle {
//                                objectName: "Faen"
                                color: activePalette.window
                                width: treeView.width
                                height: treeView.rowHeight
                            }
                        }
                    }
                    Component {
                        id: treeBranch
                        Item {
                            id: root
                            property bool isRoot: false
                            implicitHeight: column.implicitHeight
                            implicitWidth: column.implicitWidth
                            Column {
                                id: column
                                x: 2
                                Item { height: isRoot ? 0 : treeView.rowHeight; width: 1}
                                Repeater {
                                    model: elements
                                    Item {
                                        id: filler
                                        width: Math.max(loader.width + treeView.columnIndent, row.width)
                                        height: Math.max(row.height, loader.height)
                                        property var _model: model
                                        Rectangle {
                                            id: rowfill
                                            x: treeView.mapToItem(rowfill, 0, 0).x
                                            width: treeView.width
                                            height: treeView.rowHeight
                                            visible: treeView.currentNode === model
                                            color: activePalette.highlight
                                        }
                                        MouseArea {
                                            anchors.fill: rowfill
                                            onPressed: {
                                                treeView.currentNode = model
                                                treeView.currentItem = loader
                                                forceActiveFocus()
                                            }
                                        }
                                        Row {
                                            id: row
                                            Item {
                                                width: treeView.rowHeight
                                                height: treeView.rowHeight
                                                opacity: model.modelData.childCount > 0 ? 1 : 0
                                                Image {
                                                    id: expander
                                                    source: "icons/expander.png"
                                                    opacity: mouse.containsMouse ? 1 : 0.7
                                                    anchors.centerIn: parent
                                                    rotation: loader.expanded ? 90 : 0
                                                    Behavior on rotation {NumberAnimation { duration: 120}}
                                                }
                                                MouseArea {
                                                    id: mouse
                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    onClicked: loader.expanded = !loader.expanded
                                                }
                                            }
                                            Loader {
                                                property var model: _model
                                                sourceComponent: treeView.delegate
                                                anchors.verticalCenter: parent.verticalCenter
                                            }
                                        }
                                        Loader {
                                            id: loader
                                            x: treeView.columnIndent
                                            height: expanded ? implicitHeight : 0
                                            property var node: model.modelData
                                            property bool expanded: false
                                            property var elements: model.modelData.children
                                            property var text: model.modelData.name
                                            sourceComponent: (expanded && !!model.modelData.childCount > 0) ? treeBranch : undefined
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
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
                    model: keyModel

                    TableViewColumn {
                        role: "keyName"
                        title: qsTr("Name")
                        width: Math.round(keyArea.width*0.5)
                    }
                    TableViewColumn {
                        role: "keyValue"
                        title: qsTr("Value")
                        width: Math.round(keyArea.width*0.5)
                    }
                }
            }
            BasicRectangle {
                id: metaArea
                width: deltaKeyAreaWidth
                height: Math.round(mainRow.height*0.3)

                ScrollView {
                    id: metaAreaView
                    anchors.fill: parent
                    anchors.margins: defaultMargins
                    ListView {
                        model: metaInfoModel
                        delegate: Text {
                            color: activePalette.text
                            text: keyName + ": " + keyValue
                        }
                    }
                }
            }
            BasicRectangle {
                id: searchResultsArea
                width: deltaKeyAreaWidth
                height: Math.round(mainRow.height*0.2)
                visible: false
                Text {
                    anchors.fill: parent
                    anchors.margins: defaultMargins
                    text: "TODO:SEARCH_RESULTS"
                    color: activePalette.text
                }

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
                    id: searchResultsAreaView
                    anchors.fill: parent
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
            Label {
                text: treeView.currentNode.modelData.path
            }
        }
    }
}
