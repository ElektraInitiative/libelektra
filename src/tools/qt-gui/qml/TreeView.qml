import QtQuick 2.2
import QtQml 2.2
import QtQuick.Controls 1.1
import "TooltipCreator.js" as TooltipCreator
import "HelperFunctions.js" as Helper

//TreeView is based on code user "Jens" posted in the qt-project forum (http://qt-project.org/forums/viewthread/30521/#146845)

ScrollView {
	id: view

	anchors.fill: parent
	anchors.margins: defaultSpacing

	property var treeModel
	property int rowHeight: 19
	property int columnIndent: 22
	property var currentNode: null
	property var currentItem: null
	property int treeAreaCopyIndex
	property var currentNodePath
	property var toolTipParent: mainWindow

	signal updateIndicator()

	Component.onCompleted: forceActiveFocus()

	contentItem: Loader {
		id: content

		onLoaded: item.isRoot = true
		sourceComponent: treeBranch
		property var elements: treeModel
	}

	property Component delegate: Row {
		spacing: defaultSpacing

		Label {
			id: label

			text: rowLoaderModel === null || rowLoaderModel === undefined ? "" : rowLoaderModel.name
			color: rowLoaderModel === null || rowLoaderModel === undefined ? "transparent" : (rowLoaderModel.isNull ? guiSettings.nodeWithoutKeyColor : guiSettings.nodeWithKeyColor)
			font.weight: rowLoaderModel === null || rowLoaderModel === undefined || !rowLoaderModel.isNamespaceRoot ? Font.Normal : Font.Bold
			onColorChanged: indicator.updateIndicator()
		}
		Indicator {
			id: indicator

			signal updateIndicator()

			Component.onCompleted: view.updateIndicator.connect(updateIndicator)
			paintcolor: label.color
			width: label.font.pixelSize*0.8
			height: width
			anchors.verticalCenter: label.verticalCenter
			opacity: rowLoaderModel === null ? 0 : (rowLoaderModel.childCount > 0 && getOpacity(rowLoaderModel) === 0 ? 1 : 0)
			onUpdateIndicator: {
				if (rowLoaderModel !== null) {
					paintcolor = label.color
					opacity = rowLoaderModel === null || rowLoaderModel === undefined ? 0 : (rowLoaderModel.childCount > 0 && getOpacity(rowLoaderModel) === 0 ? 1 : 0)
				}
			}
		}
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
					height: isRoot ? 0 : rowHeight
					width: 1
				}
				Repeater {
					id: repeater
					model: elements

					Item {
						id: filler

						width: Math.max(itemLoader.width + columnIndent, row.width)
						height: Math.max(row.height, itemLoader.height)
						property var fillerModel: model

						Rectangle {
							id: rowfill

							x: view.mapToItem(rowfill, 0, 0).x
							width: Math.max(content.width + itemLoader.x, view.width)
							height: rowHeight
							visible: currentNode === fillerModel
							color: guiSettings.highlightColor
						}
						MouseArea {
							id: rowfillMouseArea

							anchors.fill: rowfill
							acceptedButtons: Qt.LeftButton | Qt.RightButton
							focus: true
							hoverEnabled: true

							onPressed: {
								mousePressed(mouse, model, itemLoader)
							}
							onDoubleClicked:{
								mousePressed(mouse, model, itemLoader)
                                guiActions.editAction.trigger()
							}
							onEntered: {
								timer.start()
							}
							onExited: TooltipCreator.destroy()

							Item {
								Timer {
									id: timer

									interval: 500
									repeat: false

									onTriggered: {
										if (rowfillMouseArea.containsMouse && !isNull)
											TooltipCreator.create(name, value, metaValue, defaultMargins, mapToItem(null, Math.min(filler.width + defaultMargins, view.width), 0).x, mapToItem(null, 0, 0).y, toolTipParent).show()
									}
								}
							}
						}
						Row {
							id: row

							Item {
								signal changed()
								Component.onCompleted: view.updateIndicator.connect(changed)
								width: rowHeight
								height: rowHeight
								opacity: getOpacity(model)
								onChanged: opacity = getOpacity(model)
								Image {
									id: expander

									source: Helper.useIconSource("arrow-right")
									sourceSize.width: 16
									sourceSize.height: 16
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
										expand(model, itemLoader)
									}
								}
							}
							Loader {
								id: rowLoader

								property var rowLoaderModel: fillerModel
								sourceComponent: delegate
								anchors.verticalCenter: parent.verticalCenter
							}
						}
						Loader {
							id: itemLoader

							x: columnIndent
							height: expanded ? implicitHeight : 0
							property var node: model
							property bool expanded: getExpanded(model)
							property var elements: model.children
							property var text: model.name
							sourceComponent: (expanded && !!model.childCount > 0) ? treeBranch : undefined
						}
					}
				}
			}
		}
	}

	function expand(model, itemLoader) {
		if (model.childCount > 0 && !model.childrenHaveNoChildren){
			itemLoader.expanded = !itemLoader.expanded
			model.isExpanded = itemLoader.expanded
			keyAreaView.selection.clear()
		}
	}

	function mousePressed(mouse, model, itemLoader) {
			currentNode = model
			currentItem = itemLoader
			keyAreaSelectedItem = null
			editKeyWindow.selectedNode = currentNode
			forceActiveFocus()
		if (mouse.button === Qt.RightButton){
			treeContextMenu.popup()
		}
		view.updateIndicator()
	}

	function getOpacity(model) {
		if (model.childCount > 0 && !model.childrenHaveNoChildren)
			return 1
		return 0
	}

	function getExpanded(model) {
		if (model.isExpanded && model.childrenHaveNoChildren)
			return false
		return model.isExpanded
	}
}
