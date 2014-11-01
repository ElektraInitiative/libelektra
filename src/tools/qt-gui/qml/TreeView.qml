import QtQuick 2.2
import QtQuick.Controls 1.1

//TreeView is based on code user "Jens" posted in the qt-project forum (http://qt-project.org/forums/viewthread/30521/#146845)

ScrollView {

    anchors.fill: parent
    anchors.margins: defaultSpacing

    property var model: externTreeModel
    property int rowHeight: 19
    property int columnIndent: 22
    property var currentNode: null
    property var currentItem: null
    property int treeAreaCopyIndex
    property var currentNodePath

    property Component delegate: Label {
        id: label
        text: model === null ? "" : model.name
        color: model === null ? "transparent" : (model.isNull ? disabledPalette.text : activePalette.text)
    }

    contentItem: Loader {
        id: content

        onLoaded: item.isRoot = true
        sourceComponent: treeBranch
        property var elements: treeView.model

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
                    Item {
                        height: isRoot ? 0 : treeView.rowHeight
                        width: 1
                    }
                    Repeater {
                        id: repeater
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
                            Keys.onPressed: {

                                if(event.key === Qt.Key_Space){
                                    if(model.childCount > 0 && !model.childrenHaveNoChildren){
                                        loader.expanded = !loader.expanded
                                        model.isExpanded = loader.expanded
                                    }
                                }
                                else if(event.key === Qt.Key_Up){
                                    //console.log("up")

                                }
                                else if(event.key === Qt.Key_Down){
                                    //console.log("down")
                                }
                            }
                            MouseArea {
                                anchors.fill: rowfill
                                acceptedButtons: Qt.LeftButton | Qt.RightButton
                                focus: true
                                onPressed: {
                                    if(mouse.button == Qt.LeftButton){
                                        treeView.currentNode = model
                                        treeView.currentItem = loader
                                        keyAreaSelectedItem = null
                                        editKeyWindow.selectedNode = treeView.currentNode
                                        forceActiveFocus()

                                        if (treeView.currentNode !== null){
                                            if(treeView.currentNode.childCount > 0 && treeView.currentNode.childrenHaveNoChildren)
                                                keyAreaModel = treeView.currentNode.children
                                            else
                                                keyAreaModel = null
                                        }
                                    }
                                    else if(mouse.button == Qt.RightButton)
                                        treeContextMenu.popup()
                                }
                                onDoubleClicked:{
                                    if(!treeView.currentNode.isNull){
                                        editKeyWindow.selectedNode = treeView.currentNode
                                        editKeyWindow.show()
                                        editKeyWindow.populateMetaArea()
                                    }
                                }
                            }
                            Row {
                                id: row

                                Item {
                                    width: treeView.rowHeight
                                    height: treeView.rowHeight
                                    opacity: model.childCount > 0 && !model.childrenHaveNoChildren ? 1 : 0

                                    Image {
                                        id: expander

                                        source: "icons/arrow-right.png"
                                        opacity: mouse.containsMouse ? 1 : 0.7
                                        anchors.centerIn: parent
                                        rotation: loader.expanded ? 90 : 0

                                        Behavior on rotation {
                                            NumberAnimation {
                                                duration: 120
                                            }
                                        }
                                    }
                                    MouseArea {
                                        id: mouse

                                        anchors.fill: parent
                                        hoverEnabled: true

                                        onClicked: {
                                            if(model.childCount > 0 && !model.childrenHaveNoChildren){
                                                loader.expanded = !loader.expanded
                                                model.isExpanded = loader.expanded
                                            }
                                        }
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
                                property var node: model
                                property bool expanded: model.isExpanded
                                property var elements: model.children
                                property var text: model.name
                                sourceComponent: (expanded && !!model.childCount > 0) ? treeBranch : undefined
                            }
                        }
                    }
                }
            }
        }
    }
}
