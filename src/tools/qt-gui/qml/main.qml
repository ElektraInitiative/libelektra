import QtQuick 2.2
import QtQuick.Controls 1.1
import QtQuick.Window 2.0
import QtQuick.Controls.Styles 1.1
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.1
import "MainFunctions.js" as MFunctions

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

	//**Properties*********************************************************************************************//

	property int    keyAreaHeight: Math.ceil(mainRow.height*0.7 - defaultSpacing)
	property int    deltaKeyAreaHeight: Math.ceil(mainRow.height*0.5 - defaultSpacing)
	property int    keyAreaWidth: Math.ceil(mainRow.width*0.7 - defaultSpacing)
	property int    metaAreaHeight: Math.floor(mainRow.height*0.3)
	property int    deltaMetaAreaHeight: Math.floor(mainRow.height*0.25 - defaultSpacing)
	property int    searchResultsAreaHeight: Math.ceil(mainRow.height*0.25)
	property var    keyAreaSelectedItem: null
	property var    searchResultsSelectedItem: null
	property var    metaAreaModel: (keyAreaSelectedItem === null ? null : keyAreaSelectedItem.metaValue)
	property var    keyAreaModel
	property bool   isPasted
	property bool	error: false

	//Spacing & Margins recommended by KDE HIG
	property int    defaultMargins: 8
	property int    defaultSpacing: defaultMargins*0.5

	//**Signals & Slots****************************************************************************************//

	//Set up slots to catch signals from objects
	Connections {
		target: externTreeModel

		onShowMessage: {
			MFunctions.showMessage(title, text, detailedText)
		}
	}

	Connections {
		target: guiBackend

		onShowMessage: {
			MFunctions.showMessage(title, text, detailedText)
		}
	}

	Connections {
		target: treeView.currentNode === null ? null : treeView.currentNode.node

		onShowMessage: {
			MFunctions.showMessage(title, text, detailedText)
		}
	}

	Connections {
		target: (keyAreaSelectedItem === null || keyAreaSelectedItem === 'undefined') ? null : keyAreaSelectedItem.node

		onShowMessage: {
			MFunctions.showMessage(title, text, detailedText)
		}
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
		//in order to execute different actions for the same key, these actions have to be defined here and not inside the button
		addButton.onClicked: {
			//add visual item
			qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
		}
	}

	EditKeyWindow {
		id: editKeyWindow

		onVisibleChanged: {
			if(visible === true){
				if(nameTextField.text.charAt(0) === '#'){
					editKeyWindow.title = qsTr("Edit Array Entry")
					nameTextField.textColor = disabledPalette.text
					nameTextField.readOnly = true
					valueTextField.forceActiveFocus()
				}
			}
		}
		//in order to execute different actions for the same key, these actions have to be defined here and not inside the button
		addButton.onClicked: {
			//add visual item
			qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
		}
	}

	NewKeyWindow {
		id: newArrayWindow

		title: qsTr("Create new Array Entry")

		onVisibleChanged: {
			if(visible === true){
				nameTextField.readOnly = true
				nameTextField.textColor = disabledPalette.text
				nameTextField.text = treeView.currentNode.children.getCurrentArrayNo()
				valueTextField.forceActiveFocus()
			}
		}

		addButton.onClicked: {
			//add visual item
			qmlMetaKeyModel.append({"metaName" : "", "metaValue" : ""})
		}
	}

	UnmountBackendWindow {
		id: unmountBackendWindow
		okButton.onClicked: unmountBackendWindow.close()
		cancelButton.onClicked: unmountBackendWindow.close()
	}

	WizardLoader {
		id: wizardLoader
	}

	AboutWindow {
		id: aboutWindow
	}

	//**Dialogs************************************************************************************************//

	ExportDialog {
		id: exportDialog
	}

	ImportDialog {
		id: importDialog
	}

	GeneralMessageDialog {
		id: errorDialog
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
		enabled: treeView.currentItem !== null
		onTriggered: newKeyWindow.show()
	}

	Action {
		id:newArrayAction

		iconSource: "icons/new-array.png"
		text: qsTr("Array Entry...")
		enabled: treeView.currentItem !== null
		onTriggered: newArrayWindow.show()
	}

	Action {
		id:deleteAction

		text: qsTr("Delete")
		iconSource: "icons/delete.png"
		tooltip: "Delete"
		shortcut: StandardKey.Delete
		enabled: !(searchResultsSelectedItem === null && treeView.currentNode === null && keyAreaSelectedItem === null)

		onTriggered: {
			if(searchResultsSelectedItem !== null)
				MFunctions.deleteSearchResult()
			else if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.deleteBranch()
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.deleteKey()
		}
	}

	Action {
		id: importAction

		text: qsTr("Import Configuration... ")
		iconSource: "icons/import.png"
		tooltip: qsTr("Import Configuration")
		enabled: treeView.currentItem !== null
		onTriggered: importDialog.show()
	}

	Action {
		id: exportAction

		text: qsTr("Export Configuration... ")
		iconSource: "icons/export.png"
		tooltip: qsTr("Export Configuration")
		enabled: treeView.currentItem !== null
		onTriggered: {
			exportDialog.nameFilters = guiBackend.nameFilters()
			exportDialog.open()
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
				//				MFunctions.resetKeyAreaModel()

				//				if(keyAreaModel !== null)
				//					keyAreaModel.refresh()

				//				externTreeModel.refresh()
			}
			else if(undoManager.undoText === "deleteBranch"){
				undoManager.undo()
				//				if(keyAreaModel !== null)
				//					keyAreaModel.refresh()
				externTreeModel.refresh()
			}
			else if(undoManager.undoText === "deleteSearchResultsKey" || undoManager.undoText === "deleteSearchResultsBranch"){
				undoManager.undo()
				undoManager.undo()
			}
			else if(undoManager.undoText === "copyKey"){
				undoManager.undo()

				//				if(keyAreaView.currentRow >= keyAreaModel.count()) {
				//					metaAreaModel = null
				//					keyAreaSelectedItem = null
				//					keyAreaModel.refresh()
				//				}
			}
			else if(undoManager.undoText === "copyBranch"){
				undoManager.undo()
				externTreeModel.refresh()
			}
			else if(undoManager.undoText === "cutKey"){
				undoManager.undo()
			}
			else if(undoManager.undoText === "cutBranch"){
				undoManager.undo()
				externTreeModel.refresh()
			}
			else if(undoManager.undoText === "import"){
				undoManager.undo()
				externTreeModel.refresh()
			}
			else if(undoManager.undoText === "newKey"){
				undoManager.undo()
				externTreeModel.refresh()
				//				keyAreaView.selection.clear()
			}
			else{
				undoManager.undo()
				//				keyAreaView.selection.clear()
				if(searchResultsListView.model !== null && searchResultsListView.model !== undefined)
					searchResultsListView.model.refresh()
			}
		}
	}

	Action {
		id: undoAllAction

		text: qsTr("Undo All")
		tooltip: qsTr("Undo All")
		enabled: undoManager.canUndo
		onTriggered: {
			//cannot use UndoStack::setIndex() because View-Updates would get lost
			for(var i = undoManager.index(); i > undoManager.cleanIndex(); i--)
				undoManager.undo()
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
				undoManager.redo()
				//				metaAreaModel = null
				//				externTreeModel.refresh()
			}
			else if(undoManager.redoText === "deleteBranch"){
				undoManager.redo()

				//				if(metaAreaModel !== null)
				//					metaAreaModel = null

				//				if(keyAreaSelectedItem !== null)
				//					keyAreaSelectedItem = null

				externTreeModel.refresh()

			}
			else if(undoManager.redoText === "deleteSearchResultsKey" || undoManager.redoText === "deleteSearchResultsBranch"){
				undoManager.redo()
				undoManager.redo()
			}
			else if(undoManager.redoText === "copyKey"){
				undoManager.redo()
				//				keyAreaModel.refresh()
			}
			else if(undoManager.redoText === "copyBranch"){
				undoManager.redo()
				externTreeModel.refresh()
				//				MFunctions.resetKeyAreaModel()
			}
			else if(undoManager.redoText === "cutKey"){
				undoManager.redo()
			}
			else if(undoManager.redoText === "cutBranch"){
				undoManager.redo()
				externTreeModel.refresh()
			}
			else if(undoManager.redoText === "import"){
				undoManager.redo()
				externTreeModel.refresh()
			}
			else if(undoManager.redoText === "newKey"){
				undoManager.redo()
				externTreeModel.refresh()
			}
			else{
				undoManager.redo()
				if(searchResultsListView.model !== null && searchResultsListView.model !== undefined)
					searchResultsListView.model.refresh()
			}
		}
	}

	Action {
		id: redoAllAction

		text: qsTr("Redo All")
		tooltip: qsTr("Redo All")
		enabled: undoManager.canRedo
		onTriggered: {
			//cannot use UndoStack::setIndex() because View-Updates would get lost
			for(var i = undoManager.index(); i < undoManager.count(); i++)
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
		onTriggered: {
			wizardLoader.usedNames = guiBackend.mountPoints()
			wizardLoader.show()
		}
	}

	Action {
		id: unmountBackendAction

		text: qsTr("Unmount Backend...")
		tooltip: qsTr("Unmount Backend")
		onTriggered: {
			unmountBackendWindow.mountedBackendsView.model = externTreeModel.mountedBackends()
			unmountBackendWindow.mountedBackendsView.currentIndex = -1
			unmountBackendWindow.show()
		}
	}

	Action {
		id: editAction

		iconSource: "icons/edit-rename.png"
		text: qsTr("Edit...")
		tooltip: qsTr("Edit")
		enabled: !((treeView.currentNode === null || treeView.currentNode.isNull) && keyAreaSelectedItem === null)

		onTriggered: {
			if(editKeyWindow.accessFromSearchResults){
				editKeyWindow.selectedNode = searchResultsListView.model.get(searchResultsListView.currentIndex)
			}

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
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null)

		onTriggered: {
			if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.cutBranch()
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.cutKey()
		}
	}

	Action {
		id: copyAction

		iconSource: "icons/edit-copy.png"
		text: qsTr("Copy")
		tooltip: qsTr("Copy")
		shortcut: StandardKey.Copy
		enabled: !(treeView.currentNode === null && keyAreaSelectedItem === null)

		onTriggered: {
			if(treeView.currentNode !== null && keyAreaSelectedItem === null)
				MFunctions.copyBranch()
			else if(treeView.currentNode !== null && keyAreaSelectedItem !== null)
				MFunctions.copyKey()
		}
	}

	Action {
		id: pasteAction

		iconSource: "icons/edit-paste.png"
		text: qsTr("Paste")
		tooltip: qsTr("Paste")
		shortcut: StandardKey.Paste
		enabled: undoManager.canPaste

		onTriggered: MFunctions.paste()
	}

	Action {
		id: aboutAction
		text: qsTr("About Elektra Editor")
		iconSource: "icons/elektra-logo.png"
		onTriggered: aboutWindow.show()
	}

	//**Menus & Toolbars***************************************************************************************//

	menuBar: MainMenuBar {
		id:mainMenuBar
	}

	toolBar: MainToolBar {
		id:mainToolbar
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
		}
		MenuItem {
			id: tcmPaste

			action: pasteAction
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

	//Search Results Area Context Menu

	Menu {
		id: searchResultsContextMenu

		MenuItem {
			id: srcmEdit
			action: editAction
		}
		MenuItem {
			id: srcmDelete
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

			width: Math.ceil(parent.width*0.3)
			height: parent.height
			//			border.color: treeView.activeFocus ? activePalette.highlight : activePalette.dark

			TreeView {
				id: treeView
			}
		}
		Column {
			id: keyMetaColumn

			spacing: defaultSpacing

			BasicRectangle {
				id: keyArea

				width: keyAreaWidth
				height: keyAreaHeight

				//				border.color: keyAreaView.activeFocus ? activePalette.highlight : activePalette.dark

				Component {
					id: tableViewColumnDelegate

					Item {
						anchors.fill: parent
						anchors.margins: defaultMargins

						Text{
							anchors.verticalCenter: parent.verticalCenter
							text: (treeView.currentNode === null || styleData.value === undefined) ? "" : styleData.value.replace(/\n/g, " ")
							color: treeView.currentNode === null ? "transparent" : ((keyAreaView.keyAreaCopyIndex === styleData.row && treeView.currentNode.path === keyAreaView.currentNodePath && keyAreaSelectedItem !== null) ? disabledPalette.text : activePalette.text)
						}
					}
				}

				TableView {
					id: keyAreaView

					property int keyAreaCopyIndex:-1
					property string currentNodePath:""

					anchors.fill: parent
					anchors.margins: 1
					frameVisible: false
					alternatingRowColors: false
					backgroundVisible: false

					model: keyAreaModel

					TableViewColumn {
						id: nameColumn

						role: "name"
						title: qsTr("Key Name")
						width: Math.ceil(keyArea.width*0.5 - defaultSpacing*0.5)
						delegate: tableViewColumnDelegate
					}
					TableViewColumn {
						id: valueColumn

						role: "value"
						title: qsTr("Key Value")
						width: Math.ceil(keyArea.width*0.5 - defaultSpacing*0.5)
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
									else if(mouse.button === Qt.LeftButton){
										keyAreaView.currentRow = styleData.row
										MFunctions.updateKeyAreaSelection()
									}
								}

								onDoubleClicked: {

									keyAreaView.currentRow = styleData.row
									MFunctions.updateKeyAreaSelection()
									editKeyWindow.qmlMetaKeyModel.clear()
									editKeyWindow.populateMetaArea()
									editKeyWindow.show()
								}
							}
						}
					}
					Keys.onPressed: {

						if(event.key === Qt.Key_Up) {
							keyAreaView.currentRow = keyAreaView.currentRow--
							MFunctions.updateKeyAreaSelection()
						}
						else if(event.key === Qt.Key_Down){
							keyAreaView.currentRow = keyAreaView.currentRow++
							MFunctions.updateKeyAreaSelection()
						}
						else if((event.key === Qt.Key_Enter || event.key === Qt.Key_Return) && keyAreaSelectedItem !== null){
							MFunctions.updateKeyAreaSelection()
							editKeyWindow.show()
							editKeyWindow.populateMetaArea()
						}
					}
				}
			}
			BasicRectangle {
				id: metaArea

				width: keyAreaWidth
				height: metaAreaHeight

				//				border.color: metaAreaTableView.activeFocus ? activePalette.highlight : activePalette.dark

				TableView {
					id: metaAreaTableView

					anchors.fill: parent
					anchors.margins: 1
					frameVisible: false
					alternatingRowColors: false
					backgroundVisible: false
					selectionMode: SelectionMode.NoSelection

					model: metaAreaModel

					TableViewColumn {
						id: metaNameColumn

						role: "name"
						title: qsTr("Metakey Name")
						width: Math.ceil(metaArea.width*0.5 - defaultSpacing*0.5)
					}
					TableViewColumn {
						id: metaValueColumn

						role: "value"
						title: qsTr("Metakey Value")
						width: Math.ceil(metaArea.width*0.5 - defaultSpacing*0.5)
					}
				}
			}
			BasicRectangle {
				id: searchResultsArea

				width: keyAreaWidth
				height: searchResultsAreaHeight
				visible: false

				SequentialAnimation {
					id: searchResultsColorAnimation

					ColorAnimation {
						target: searchResultsArea
						property: "border.color"
						to: (searchResultsListView.model !== null && searchResultsListView.model.get(0).name === "NotfoundNode") ? "red" : "green"
						duration: 0
					}
					ColorAnimation {
						target: searchResultsArea
						property: "border.color"
						to: activePalette.dark
						duration: 1000
					}
				}

				ToolButton {
					id: searchResultsCloseButton

					iconSource: "icons/dialog-close.png"
					anchors.right: parent.right
					anchors.top: parent.top
					anchors.margins: defaultSpacing
					tooltip: qsTr("Close")
					onClicked: {
						keyMetaColumn.state = ""
						searchResultsSelectedItem = null
					}
				}

				ScrollView {
					id: searchResultsScrollView

					anchors.fill: parent
					anchors.margins: defaultMargins
					anchors.rightMargin: searchResultsCloseButton.width

					ListView {
						id: searchResultsListView

						anchors.fill: parent
						clip: true
						highlightMoveDuration: 0
						highlightResizeDuration: 0
						keyNavigationWraps: true
						model: null

						Keys.onPressed: {
							if(event.key === Qt.Key_Up && searchResultsListView.currentIndex > 0){
								currentIndex--
								searchResultsSelectedItem = model.get(currentIndex)
							}
							else if(event.key === Qt.Key_Down && searchResultsListView.currentIndex < model.count() - 1){
								currentIndex++
								searchResultsSelectedItem = model.get(currentIndex)
							}
							else if((event.key === Qt.Key_Enter || event.key === Qt.Key_Return) && searchResultsSelectedItem !== null){
								editKeyWindow.selectedNode = searchResultsSelectedItem
								editKeyWindow.accessFromSearchResults = true
								editKeyWindow.show()
								editKeyWindow.populateMetaArea()
							}
						}

						highlight: Rectangle {
							id: highlightBar
							color: activePalette.highlight
						}

						delegate: Text {
							color: activePalette.text
							text: path

							MouseArea {
								anchors.fill: parent
								acceptedButtons: Qt.LeftButton | Qt.RightButton

								onClicked: {
									if(searchResultsListView.model.get(0).name !== "NotfoundNode"){
										if(mouse.button === Qt.LeftButton){
											searchResultsListView.currentIndex = index
											searchResultsSelectedItem = searchResultsListView.model.get(searchResultsListView.currentIndex)
											forceActiveFocus()
										}
										else if(mouse.button === Qt.RightButton) {
											searchResultsListView.currentIndex = index
											searchResultsSelectedItem = searchResultsListView.model.get(searchResultsListView.currentIndex)
											forceActiveFocus()
											editKeyWindow.accessFromSearchResults = true
											searchResultsContextMenu.popup()
										}
									}
								}
								onDoubleClicked: {
									if(searchResultsListView.model.get(0).name !== "NotfoundNode"){
										searchResultsListView.currentIndex = index
										forceActiveFocus()
										editKeyWindow.accessFromSearchResults = true
										editKeyWindow.selectedNode = searchResultsListView.model.get(searchResultsListView.currentIndex)
										editKeyWindow.show()
										editKeyWindow.populateMetaArea()
									}
								}
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
				text: treeView.currentNode === null ? "" : treeView.currentNode.path + (keyAreaSelectedItem === null ? "" : "/" + keyAreaSelectedItem.name)
			}
		}
	}
}
