@INCLUDE_COMMON@

echo
echo ELEKTRA PLUGIN SOURCE CHECK TEST SUITE
echo

check_version

PLUGIN_DIR="@CMAKE_SOURCE_DIR@/src/plugins"

for PLUGIN in `ls "$PLUGIN_DIR"`
do
	if [ ! -d "$PLUGIN_DIR/$PLUGIN" ]; then
		continue
	fi
	echo "Do in-source check for plugin $PLUGIN"

	README="$PLUGIN_DIR/README.md"
	WC=`grep "^- \\[$PLUGIN\\]($PLUGIN.*)" "$README"  | wc -l`
	[ $WC -eq 1 ]
	succeed_if "Your plugin $PLUGIN does not have an entry ^- [$PLUGIN]($PLUGIN)$ in $README"

	CACHE=@CMAKE_SOURCE_DIR@/cmake/ElektraCache.cmake
	WC=`grep "^	$PLUGIN$" "$CACHE"  | wc -l`
	[ $WC -eq 1 ]
	succeed_if "Your plugin $PLUGIN does not have an entry ^<tab>$PLUGIN$ in $CACHE"
done

end_script basic commands
