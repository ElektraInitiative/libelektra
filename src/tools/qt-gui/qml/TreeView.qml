import QtQuick 2.2
import QtQuick.Controls 1.1

//TreeView is based on code user "Jens" posted in the qt-project forum (http://qt-project.org/forums/viewthread/30521/#146845)

ScrollView {

	anchors.fill: parent
	anchors.margins: defaultSpacing

	property var treeModel: externTreeModel
	property int rowHeight: 19
	property int columnIndent: 22
	property var currentNode: null
	property var currentItem: null
	property int treeAreaCopyIndex
	property var currentNodePath

	property Component delegate: Label {
		id: label

		text: rowLoaderModel === null ? "" : rowLoaderModel.name
		color: rowLoaderModel === null ? "transparent" : (rowLoaderModel.isNull ? disabledPalette.text : activePalette.text)
	}

	contentItem: Loader {
		id: content

		onLoaded: item.isRoot = true
		sourceComponent: treeBranch
		property var elements: treeView.treeModel
	}

	property Component treeBranch: Component {
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

						width: Math.max(itemLoader.width + treeView.columnIndent, row.width)
						height: Math.max(row.height, itemLoader.height)
						property var fillerModel: model

						Rectangle {
							id: rowfill

							x: treeView.mapToItem(rowfill, 0, 0).x
							width: treeView.width
							height: treeView.rowHeight
							visible: treeView.currentNode === fillerModel
							color: activePalette.highlight
						}
						Keys.onPressed: {
							if(event.key === Qt.Key_Space){
								treeView.currentNode = model
								treeView.currentItem = itemLoader
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
									treeView.currentItem = itemLoader
									keyAreaSelectedItem = null
									metaAreaModel = null
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
									rotation: itemLoader.expanded ? 90 : 0

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
											itemLoader.expanded = !itemLoader.expanded
											model.isExpanded = itemLoader.expanded
										}
									}
								}
							}
							Loader {
								id: rowLoader

								property var rowLoaderModel: fillerModel
								sourceComponent: treeView.delegate
								anchors.verticalCenter: parent.verticalCenter
							}
						}
						Loader {
							id: itemLoader

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
