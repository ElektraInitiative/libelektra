function useIcon(name) {
  if (!guiSettings.useSystemIconTheme) return "";
  return name;
}

function useIconSource(name) {
  if (!guiSettings.useSystemIconTheme) return "icons/".concat(name, ".png");
  return "image://theme/".concat(name);
}
