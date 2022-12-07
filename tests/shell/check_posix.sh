#!/bin/sh

@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK SHELL POSIX TEST
echo

command -v shfmt > /dev/null 2>&1 || {
	echo "shfmt command needed for this test, aborting" >&2
	exit 0
}

cd "@CMAKE_SOURCE_DIR@" || exit

for file in $(shfmt -f scripts tests/shell); do
	head -n 10 "$file" | grep -qe '#!.*bash' && continue
	shfmt -p "$file" > /dev/null
	exit_if_fail "Possible non-POSIX code in Shell scripts detected, please check."
done

end_script
