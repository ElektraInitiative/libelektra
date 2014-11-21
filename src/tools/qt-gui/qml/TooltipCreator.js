.pragma library

var component = Qt.createComponent("ToolTip.qml");

function create(name, value, meta, defaultMargins, x, y, parent) {

	var properties = {}

	properties.name = name
	properties.value = value
	properties.meta = meta
	properties.defaultMargins = defaultMargins

	var tooltip = component.createObject(parent, properties);

	if (tooltip === null)
		console.error("error creating tooltip: " + component.errorString());

	tooltip.x = x
	tooltip.y = y
	tooltip.z = parent.z + 1

	return tooltip;
}

