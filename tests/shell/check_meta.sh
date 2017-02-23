@INCLUDE_COMMON@

echo
echo ELEKTRA PLUGIN METATADA CONSISTENCY CHECK
echo

PLUGINS_DIR="@CMAKE_SOURCE_DIR@/src/plugins"
META_FILE="@CMAKE_SOURCE_DIR@/doc/METADATA.ini"

for PLUGIN in `ls "$PLUGINS_DIR"`;
do

    if [ ! -d "${PLUGINS_DIR}/${PLUGIN}" ];
    then
	continue
    fi
    
    README="${PLUGINS_DIR}/${PLUGIN}/README.md"
    PLUGIN_META=$(grep -Eo "infos/metadata(.*)" "$README" |cut -d '=' -f2)
    for META in $PLUGIN_META;
    do
	grep -Eq "^\\[${META}\\]$" "$META_FILE"
        succeed_if "Metadata $META of plugin $PLUGIN not present in METADATA.ini"
    done
    USED_BY=$(awk "/usedby\\/plugin= ([^\\n]*)${PLUGIN}( |\\n)/" RS= "$META_FILE")
    USED_BY_META=$(echo "$USED_BY" | grep -Eo "^\\[.*\\]$")

    OLD_IFS="$IFS"
    IFS="$(printf '\n+')"

    for META in $USED_BY_META;
    do
	STRIPPED_META=$(echo "$META" | sed 's/\[//g' | sed 's/\]//g')
	grep -Eq "infos/metadata(.*)${STRIPPED_META}" "$README"
	succeed_if "$STRIPPED_META should be used by $PLUGIN, but not present in ${PLUGIN}/README.md infos/metadata"
    done

    IFS="$OLD_IFS"
done

end_script
