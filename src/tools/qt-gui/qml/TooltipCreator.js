.pragma library

var component = Qt.createComponent("ToolTip.qml")
var tooltip = null

function create(name, value, meta, defaultMargins, x, y, parent) {

	var properties = {}

	properties.name = name
	properties.value = value
	properties.meta = meta
	properties.defaultMargins = defaultMargins
	properties.parentWidth = parent.width
	properties.parentHeight = parent.height

	tooltip = component.createObject(parent, properties);

	if (tooltip === null)
		console.error("error creating tooltip: " + component.errorString());

	tooltip.x = x
	tooltip.y = y

	return tooltip;
}

function destroy() {
	if(tooltip){
		tooltip.destroy()
		tooltip = null
	}
}

