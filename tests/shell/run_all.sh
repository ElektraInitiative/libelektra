@INCLUDE_COMMON@

echo
echo RUN ALL TESTS
echo

check_version

cd "@CMAKE_INSTALL_PREFIX@/@TARGET_TOOL_EXEC_FOLDER@"

for t in test*
do
	$KDB $t
	succeed_if "$t failed"
done

for t in check_*
do
	$KDB $t
	succeed_if "$t failed"
done

end_script all
