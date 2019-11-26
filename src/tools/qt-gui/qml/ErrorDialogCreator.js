//display an error message dialog

function showMessage(title, text, detailedText, parent) {
  var dialog = null;
  var component = Qt.createComponent("ErrorDialog.qml");
  var properties = {};

  properties.title = title;
  properties.errorText = text;
  properties.detailedText = detailedText;
  properties.visible = true;

  dialog = component.createObject(null, properties);

  if (dialog === null)
    console.error("error creating errordialog: " + component.errorString());

  error = true;
  return dialog;
}
