#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA PLUGIN METATADA CONSISTENCY CHECK
echo

PLUGINS_DIR="@CMAKE_SOURCE_DIR@/src/plugins"
META_FILE="@CMAKE_SOURCE_DIR@/doc/METADATA.ini"

for PLUGIN in $(ls "$PLUGINS_DIR"); do

	if [ ! -d "${PLUGINS_DIR}/${PLUGIN}" ]; then
		continue
	fi

	README="${PLUGINS_DIR}/${PLUGIN}/README.md"
	PLUGIN_META=$(grep -Eo "infos/metadata(.*)" "$README" | cut -d '=' -f2)
	for META in $PLUGIN_META; do
		grep -Eq "^\\[${META}\\]$" "$META_FILE"
		succeed_if "Metadata $META is in infos/metadata of ${PLUGINS_DIR}/$PLUGIN/README.md, but not present in $META_FILE for $PLUGIN"
	done

	USED_BY=$(awk "/usedby\\/plugin=([^\\n]*) ${PLUGIN}( |\\n)/" RS= "$META_FILE")
	USED_BY_META=$(echo "$USED_BY" | grep -Eo "^\\[.*\\]$")

	OLD_IFS="$IFS"
	IFS="$(printf '\n+')"

	for META in $USED_BY_META; do
		STRIPPED_META=$(echo "$META" | sed 's/\[//g' | sed 's/\]//g')
		grep -Eq "infos/metadata(.*)${STRIPPED_META}" "$README"
		succeed_if "Metadata $STRIPPED_META is in $META_FILE for $PLUGIN, but not present in infos/metadata of ${PLUGINS_DIR}/${PLUGIN}/README.md"
	done

	IFS="$OLD_IFS"
done

end_script
