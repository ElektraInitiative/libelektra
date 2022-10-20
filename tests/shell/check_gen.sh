#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK GEN
echo

while getopts ":qk" opt; do
	case $opt in
	q)
		nodiff=1
		;;
	k)
		keeplogs=1
		;;
	*) ;;

	esac
done

echo "Using $KDB"

check_version

if is_plugin_available ni; then
	echo "ni plugin available"
else
	echo "no ni plugin available"
	exit
fi

if is_plugin_available type; then
	echo "type plugin available"
else
	echo "no type plugin available"
	exit
fi

if is_plugin_available reference; then
	echo "reference plugin available"
else
	echo "no reference plugin available"
	exit
fi

base_output_folder="@CMAKE_CURRENT_BINARY_DIR@/gen"

for test_folder in "@CMAKE_SOURCE_DIR@"/tests/shell/gen/*/; do
	[ -e "$test_folder" ] || continue
	output_folder="$base_output_folder/$(basename "$test_folder")/"
	mkdir -p "$output_folder"

	template=$(basename "$test_folder")
	echo "testing template $template"
	for test_path in "$test_folder"*.data.ini; do
		[ -e "$test_path" ] || continue

		test_file=${test_path##*/}
		test_name=${test_file%.data.ini}

		test_params=$(cat "$test_folder/$test_name.params")

		cascading_parent="$MOUNTPOINT/gen/$template/$test_name"
		spec_parent="spec:$cascading_parent"
		user_parent="user:$cascading_parent"

		echo "running test $test_name with parent key $spec_parent"

		"$KDB" mount "$test_file.spec.ini" "$spec_parent" ni
		succeed_if "couldn't mount data"
		"$KDB" mount "$test_file.user.ini" "$user_parent" ni
		succeed_if "couldn't mount data"

		"$KDB" import "$spec_parent" ni < "$test_path"
		succeed_if "couldn't import data"

		"$KDB" spec-mount "$cascading_parent"
		succeed_if "couldn't spec-mount data"

		old_dir=$(pwd)
		cd "$output_folder" || exit 1
		# shellcheck disable=SC2086
		"$KDB" gen "$template" "$cascading_parent" "$test_name.actual" ${test_params} > "$output_folder$test_name.stdout" 2> "$output_folder$test_name.stderr"
		gen=$?
		if [ "$gen" != "0" ] && [ ! -e "$test_folder$test_name.stderr" ]; then
			test "1" = "0"
			succeed_if "kdb gen failed: "
			cat "$output_folder$test_name.stderr"
		fi
		cd "$old_dir" || exit 1

		if [ -e "$test_folder$test_name.stdout" ]; then
			diff -u "$test_folder$test_name.stdout" "$output_folder$test_name.stdout" | sed -e "1d" -e "2d" > "$output_folder$test_name.stdout.diff"

			if [ -s "$output_folder$test_name.stdout.diff" ]; then
				test "1" = "0"
				succeed_if "stdout of $test_name didn't match the expected output."
				if [ "$nodiff" = "" ]; then
					echo "Here is the diff:"
					cat "$output_folder$test_name.stdout.diff"
					echo
				fi
				echo "The diff is also stored at $output_folder$test_name.stdout.diff"
				echo
			else
				rm "$output_folder$test_name.stdout.diff"
			fi
		fi
		rm "$output_folder$test_name.stdout"

		if [ -e "$test_folder$test_name.stderr" ]; then
			sed -e "s#$KDB#kdb#" -e '1!b' -e '/^The command kdb gen terminated unsuccessfully with the info:$/d' "$output_folder$test_name.stderr" > "$output_folder$test_name.stderr2"
			mv "$output_folder$test_name.stderr2" "$output_folder$test_name.stderr"
			diff -u "$test_folder$test_name.stderr" "$output_folder$test_name.stderr" | sed -e "1d" -e "2d" > "$output_folder$test_name.stderr.diff"

			if [ -s "$output_folder$test_name.stderr.diff" ]; then
				test "1" = "0"
				succeed_if "stderr of $test_name didn't match the expected output."
				if [ "$nodiff" = "" ]; then
					echo "Here is the diff:"
					cat "$output_folder$test_name.stderr.diff"
					echo
				fi
				echo "The diff is also stored at $output_folder$test_name.stderr.diff"
				echo
			else
				rm "$output_folder$test_name.stderr.diff"
			fi
		fi
		rm "$output_folder$test_name.stderr"

		if [ "$gen" = "0" ]; then
			for expected_part in "$test_folder$test_name".expected*; do
				[ -e "$expected_part" ] || continue

				part=${expected_part#"$test_folder$test_name.expected"}
				actual_part="$output_folder$test_name.actual$part"
				diff_part="$output_folder$test_name$part.diff"

				[ -f "$actual_part" ]
				succeed_if "missing part $test_name.actual$part"

				diff_result=$(diff -u "$expected_part" "$actual_part")
				if [ -n "$diff_result" ]; then
					if echo "$diff_result" | grep "^Binary files"; then
						printf "" > "$diff_part"

						echo "Binary file didn't match."
						echo "The actual file is stored at $actual_part"
						echo
					else
						printf "%s\n" "$diff_result" | sed -e "1s/.*/--- $test_name.expected$part/" -e "2s/.*/+++ $test_name.actual$part/" > "$diff_part"

						test "1" = "0"
						succeed_if "$test_name.actual$part didn't match the expected output $test_name.expected$part."
						if [ "$nodiff" = "" ]; then
							echo "Here is the diff:"
							cat "$diff_part"
							echo
						fi
						echo "The diff is also stored at $diff_part"
						echo
					fi
				else
					rm -f "$diff_part"
				fi
			done

			if [ -e "$output_folder$test_name.check.sh" ]; then
				old_dir=$(pwd)
				cd "$output_folder" || exit 1

				if ! (KDB="$KDB" MOUNTPOINT="$MOUNTPOINT/gen/$template/$test_name" sh "$output_folder$test_name.check.sh" > "$output_folder$test_name.check.log" 2>&1); then
					test "1" = "0"
					succeed_if "$test_folder$test_name.check.sh didn't complete successfully"

					if [ "$nodiff" = "" ]; then
						echo "Here is the log:"
						cat "$output_folder$test_name.check.log"
						echo
					fi
					echo "The log is also stored at $output_folder$test_name.check.log"
					echo
				else
					[ "$keeplogs" = "1" ] || rm "$output_folder$test_name.check.log"
				fi

				cd "$old_dir" || exit 1
			fi

			for actual_part in "$output_folder$test_name".actual*; do
				[ -e "$actual_part" ] || continue

				part=${actual_part#"$output_folder$test_name.actual"}
				expected_part="$test_folder$test_name.expected$part"
				diff_part="$output_folder$test_name$part.diff"

				if [ -e "$diff_part" ]; then
					if [ -s "$diff_part" ]; then
						rm "$actual_part"
						continue
					else
						rm "$diff_part"
					fi
				elif [ -e "$expected_part" ]; then
					rm "$actual_part"
					continue
				fi

				test "1" = "0"
				succeed_if "additional part ${actual_part}"
			done
		else
			rm "$output_folder$test_name".actual*
		fi

		"$KDB" rm -r "$cascading_parent"

		rm -f "$("$KDB" file "$spec_parent")"
		rm -f "$("$KDB" file "$user_parent")"

		"$KDB" umount "$cascading_parent"
		"$KDB" umount "$spec_parent"
		"$KDB" umount "$user_parent"
		echo "----------------------------------------------------------------------"
	done
	echo "======================================================================"
done

end_script
