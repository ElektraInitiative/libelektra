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

for PLUGIN in $PLUGINS
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
	esac

	> $FILE
	"$KDB" check $PLUGIN 1> $FILE 2> $FILE
	succeed_if "check of plugin $PLUGIN failed"

	test ! -s $FILE
	succeed_if "check of plugin $PLUGIN produced: \"`cat $FILE`\""
done

end_script basic commands
