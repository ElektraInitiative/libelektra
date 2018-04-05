@INCLUDE_COMMON@

echo
echo RUN ALL TESTS
echo

check_version

cd "@CMAKE_INSTALL_PREFIX@/@TARGET_TOOL_EXEC_FOLDER@"

nbTests="0"
nbFailed=""

cleanup()
{
	rm -rf "$EXPORT_DIR"
}

EXPORT_DIR="$(mktempdir_elektra)"
export_config "$EXPORT_DIR"

for t in test* check*
do
	echo "--- running $t ---"
	echo
	echo

	"$KDB" $t

	if [ $? == "@SKIP_RETURN_CODE" ]
	then
		nbSkip=$(( $nbSkip + 1 ))
	else
		if [ $? != "0" ]
		then
			nbError=$(( $nbError + 1 ))
			nbFailed="$nbFailed$t"
			echo error: $t
		fi
	fi
	nbTests=$(( $nbTests + 1 ))

	export_check "$EXPORT_DIR" "$t"
done


if [ $nbError != "0" ]
then
	echo "Following test cases failed: $nbFailed"
fi

# fake the number of tests:
nbTest=$nbTests

end_script all
