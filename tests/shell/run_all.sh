@INCLUDE_COMMON@

echo
echo RUN ALL TESTS
echo

check_version

cd "@CMAKE_INSTALL_PREFIX@/@TARGET_TOOL_EXEC_FOLDER@"

nbFailed=""

EXPORTS="$(mktempdir_elektra)"

cleanup()
{
	rm -rf "$EXPORTS"
}

if [ ! -d "$EXPORTS" ]
then
	echo "Failed to create temporary directory"
	exit 1
fi

SPECEXPORT="$EXPORTS/spec.export.dump"
SPECCHECK="$EXPORTS/spec.check.dump"

DIREXPORT="$EXPORTS/dir.export.dump"
DIRCHECK="$EXPORTS/dir.check.dump"

USEREXPORT="$EXPORTS/user.export.dump"
USERCHECK="$EXPORTS/user.check.dump"

SYSTEMEXPORT="$EXPORTS/system.export.dump"
SYSTEMCHECK="$EXPORTS/system.check.dump"

MOUNTEXPORT="$EXPORTS/mount.export.dump"
MOUNTCHECK="$EXPORTS/mount.check.dump"

checkFailed()
{
	echo "$t did not left $1 config in the same state at is was before!"
	echo "This means the test itself is flawed!"
	echo "You can inspect the original $1 config in $EXPORTS/$1.export.dump"
	echo "compared to situation now in $EXPORTS/$1.check.dump"
	echo
	echo "Other important recovery files are also in the directory $EXPORTS"
	echo "Please remove the $EXPORTS directory yourself after you fixed the situation, I cannot do it for you"
	exit 1
}

$KDB export spec dump > "$SPECEXPORT"
exit_if_fail "Could not export spec config"

$KDB export dir dump > "$DIREXPORT"
exit_if_fail "Could not export dir config"

$KDB export user dump > "$USEREXPORT"
exit_if_fail "Could not export user config"

$KDB export system --without-elektra dump > "$SYSTEMEXPORT"
exit_if_fail "Could not export system config"

$KDB export system/elektra/mountpoints dump > "$MOUNTEXPORT"
exit_if_fail "Could not export mount config"

for t in test* check*
do
	echo "--- running $t ---"
	echo
	echo

	$KDB $t

	if [ $? != "0" ]
	then
		nbError=$(( $nbError + 1 ))
		nbFailed="$nbFailed\n$t"
		echo error: $t
	fi
	nbTest=$(( $nbTest + 1 ))

	$KDB export spec dump > "$SPECCHECK"
	exit_if_fail "Could not export spec config"

	$KDB export dir dump > "$DIRCHECK"
	exit_if_fail "Could not export dir config"

	$KDB export user dump > "$USERCHECK"
	exit_if_fail "Could not export user config"

	$KDB export system --without-elektra dump > "$SYSTEMCHECK"
	exit_if_fail "Could not export system config"

	$KDB export system/elektra/mountpoints dump > "$MOUNTCHECK"
	exit_if_fail "Could not export mount config"


	diff "$SPECEXPORT" "$SPECCHECK"
	if [ $? != "0" ]
	then
		checkFailed spec
	fi

	diff "$DIREXPORT" "$DIRCHECK"
	if [ $? != "0" ]
	then
		checkFailed dir
	fi

	diff "$USEREXPORT" "$USERCHECK"
	if [ $? != "0" ]
	then
		checkFailed user
	fi

	diff "$SYSTEMEXPORT" "$SYSTEMCHECK"
	if [ $? != "0" ]
	then
		checkFailed system
	fi

	diff "$MOUNTEXPORT" "$MOUNTCHECK"
	if [ $? != "0" ]
	then
		checkFailed mount
	fi
done


if [ $nbError != "0" ]
then
	echo "Following test cases failed: $nbFailed"
fi

end_script all
