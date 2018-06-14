@INCLUDE_COMMON@

echo
echo ELEKTRA KDB INTERNAL TEST SUITE
echo

check_version

FILE="$(mktempfile_elektra)"
cleanup()
{
	rm -f "$FILE"
}


ACTUAL_PLUGINS=$PLUGINS
# Otherwise the test would fail as SHARED_ONLY plugins are not
# available in full and static builds
if contains $KDB "full" || contains $KDB "static"; then
	ACTUAL_PLUGINS=$ADDED_PLUGINS_WITHOUT_ONLY_SHARED
fi

for PLUGIN in $ACTUAL_PLUGINS
do
	case "$PLUGIN" in
	"tracer")
		# output on open/close
		continue
		;;
	"timeofday")
		# output on open/close
		continue
		;;
	"counter")
		# output on open/close
		continue
		;;
	"semlock")
		# exclude due to issue 1781
		continue
		;;
	esac

	# The following checks fail on an ASAN enabled build
	# See also: https://github.com/ElektraInitiative/libelektra/pull/1963
	ASAN='@ENABLE_ASAN@'
	if [ "$ASAN" = 'ON' ]; then
		case "$PLUGIN" in
		'crypto_gcrypt')
			continue
			;;
		'crypto_botan')
			continue
			;;
		'xerces')
			continue
			;;
		'jni')
			continue
			;;
		'ruby')
			continue
			;;
		esac
	fi

	> $FILE
	"$KDB" check $PLUGIN 1> $FILE 2> $FILE
	succeed_if "check of plugin $PLUGIN failed"

	test ! -s $FILE
	succeed_if "check of plugin $PLUGIN produced: \"`cat $FILE`\""
done

end_script basic commands
