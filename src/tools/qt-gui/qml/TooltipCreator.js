.pragma library

var component = Qt.createComponent("ToolTip.qml");

function create(name, value, meta, defaultMargins, parent) {

	var properties = {}

	properties.name = name
	properties.value = value
	properties.meta = meta
	properties.left = parent.right
	properties.defaultMargins = defaultMargins

	var tooltip = component.createObject(parent, properties);

	if (tooltip === null)
		console.error("error creating tooltip: " + component.errorString());

	tooltip.anchors.left = parent.right
	tooltip.anchors.leftMargin = defaultMargins
	tooltip.z = parent.z + 1

	return tooltip;
}

