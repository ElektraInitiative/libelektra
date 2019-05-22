.pragma library // lgtm [js/syntax-error]

var tooltip = null

function create(name, value, meta, defaultMargins, x, y, parent) {

	var component = Qt.createComponent("ToolTip.qml")

	var properties = {}

	properties.name = name
	properties.value = value
	properties.meta = meta
	properties.defaultMargins = defaultMargins
	properties.parentWidth = parent.width
	properties.parentHeight = parent.height

	tooltip = component.createObject(parent, properties);

	if (tooltip === null)
		console.error("error creating tooltip: " + component.errorString())

	tooltip.x = x
	tooltip.y = y

	return tooltip
}

function createHelp(helpText, defaultMargins, x, y, parent) {

	var component = Qt.createComponent("Help.qml")
	var properties = {}

	properties.helpText = helpText
	properties.defaultMargins = defaultMargins

	tooltip = component.createObject(parent, properties);

	if (tooltip === null)
		console.error("error creating tooltip: " + component.errorString())

	tooltip.x = x
	tooltip.y = y

	return tooltip
}

function destroy() {
	if (tooltip){
		tooltip.destroy()
		tooltip = null
	}
}

