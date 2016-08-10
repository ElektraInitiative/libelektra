@INCLUDE_COMMON@

echo
echo ELEKTRA PLUGIN SOURCE CHECK TEST SUITE
echo

check_version

check_wrong_export()
{
	WC=`grep -r "ELEKTRA_PLUGIN_$1" "$2" | wc -l`
	[ $WC -le 1 ]
	succeed_if "It is likely that you are resetting a function pointer $WC times in your export, please use the word ELEKTRA_PLUGIN_$1 only once in your plugin $2"
}

PLUGINS_DIR="@CMAKE_SOURCE_DIR@/src/plugins"

for PLUGIN in `ls "$PLUGINS_DIR"`
do
	PLUGIN_DIR="$PLUGINS_DIR/$PLUGIN"

	if [ ! -d "$PLUGIN_DIR" ]; then
		continue
	fi
	echo "Do in-source check for plugin $PLUGIN"

	README="$PLUGINS_DIR/README.md"
	WC=`grep "^- \\[$PLUGIN\\]($PLUGIN/)" "$README"  | wc -l`
	[ $WC -eq 1 ]
	succeed_if "Your plugin $PLUGIN does not have an entry ^- [$PLUGIN]($PLUGIN/) in $README"

	#CACHE=@CMAKE_SOURCE_DIR@/cmake/ElektraCache.cmake
	#WC=`grep "^	$PLUGIN$" "$CACHE"  | wc -l`
	#[ $WC -eq 1 ]
	#succeed_if "Your plugin $PLUGIN does not have an entry ^<tab>$PLUGIN$ in $CACHE"

	if [ $PLUGIN != doc ]; then
		check_wrong_export GET "$PLUGIN_DIR"
		check_wrong_export SET "$PLUGIN_DIR"
		check_wrong_export OPEN "$PLUGIN_DIR"
		check_wrong_export CLOSE "$PLUGIN_DIR"
		check_wrong_export ERROR "$PLUGIN_DIR"
	fi



done

end_script plugins
