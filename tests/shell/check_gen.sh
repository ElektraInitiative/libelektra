@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK GEN
echo

echo "Using $KDB"

check_version

if is_plugin_available ni; then
	echo "ni plugin available"
else
	echo "no ni plugin available"
	exit
fi

output_folder=@CMAKE_CURRENT_BINARY_DIR@/gen/

$KDB mount "${output_folder}spec-data.ini" "$SPEC_ROOT/gen" ni

for test_folder in @CMAKE_SOURCE_DIR@/tests/shell/gen/*/; do
	[ -e "$test_folder" ] || continue
	template=$(basename "$test_folder")
	echo "testing template $template"
	for test_path in ${test_folder}*.data.ini; do
		[ -e "$test_path" ] || continue

		test_file=${test_path##*/}
		test_name=${test_file%.data.ini}

		test_params=$(cat "$test_folder/$test_name.params")

		parent_key="$MOUNTPOINT/gen/$template/$test_name"

		echo "running test $test_name with parent key $parent_key"

		$KDB import "spec$parent_key" ni < "$test_path"
		succeed_if "couldn't import data"

		old_dir=$(pwd)
		cd "$output_folder"
		$KDB gen "$template" "$parent_key" "$test_name.actual" ${test_params}
		gen=$?
		[ "$gen" == "0" ]
		succeed_if "kdb gen failed"
		cd "$old_dir"

		data_list=$($KDB ls "spec$parent_key")
		if [ -n "$data_list" ]; then
			$KDB rm -r "spec$parent_key"
			succeed_if "couldn't remove data"
		fi

		if [ "$gen" != "0" ]; then
			rm ${output_folder}${test_name}.actual*
			continue
		fi

		for expected_part in ${test_folder}${test_name}.expected*; do
			[ -e "$expected_part" ] || continue

			part=${expected_part#"$test_folder$test_name.expected"}
			actual_part="$output_folder$test_name.actual$part"
			diff_part="$output_folder$test_name$part.diff"

			[ -f "$actual_part" ]
			succeed_if "missing part $test_name.actual$part"

			diff -u "$expected_part" "$actual_part" | sed -e "1c--- $test_name.expected$part" -e "2c+++ $test_name.actual$part" > "$diff_part"

			if [ -s "$diff_part" ]; then
				[ "1" == "0" ]
				succeed_if "$test_name.actual$part didn't match the expected output $test_name.expected$part. Here is the diff:"
				cat "$diff_part"
				echo
				echo "The diff is also stored at $diff_part"
				echo
			else
				rm "$diff_part"
			fi

			rm "$actual_part"
		done

		for actual_part in ${output_folder}${test_name}.actual*; do
			[ -e "$actual_part" ] || continue

			[ "1" == "0" ]
			succeed_if "additional part ${actual_part#"$output_folder"}"
			rm "$actual_part"
		done
	done
	echo
done

$KDB umount "$SPEC_ROOT/gen"
rm "${output_folder}spec-data.ini"

end_script
