@INCLUDE_COMMON@

echo
echo ELEKTRA CHECK FORMATTING
echo

command -v git > /dev/null 2>&1 || {
	printf >&2 'This test requires the `git` command, aborting test!\n\n'
	exit 0
}

cd "@CMAKE_SOURCE_DIR@"

if ! git diff --quiet; then
	printf >&2 'Source is already modified, aborting test!\n\n'
	exit 0
fi

reformat() {
	reformat_command=$1
	reformat_command_output="$(scripts/$reformat_command 2>&1)" || {
		printf >&2 -- '————————————————————————————————————————————————————————————\n'
		printf >&2 -- 'Warning — Reformatting command `%s` failed\n' "$reformat_command"
		printf >&2 -- '\n%s\n' "$reformat_command_output"
		printf >&2 -- '————————————————————————————————————————————————————————————\n\n'
	}
}

reformat reformat-source &
reformat reformat-cmake &
reformat reformat-markdown &
reformat reformat-shfmt &
wait

error_message="$(
	cat << 'EOF'
The reformatting check detected code that **does not** fit the guidelines given in `doc/CODING.md`.
If you see this message on one of the build servers, you can either install one or multiple of the following tools:

- [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) to format C and C++ source code,
- [`cmake_format`](https://github.com/cheshirekow/cmake_format) to format CMake code,
- [`prettier`](https://prettier.io) to format Markdown code, and
- [`shfmt`](https://github.com/mvdan/sh) to format Shell code

. Afterwards you can use the following scripts to fix the formatting problems

- `reformat-source` to format C/C++ source files,
- `reformat-cmake` to format CMake files,
- `reformat-markdown` to format Markdown files, and
- `reformat-shfmt` to format files that contain shell code

. If you do not want to install any of the tools listed above you can also use the `patch` command after this message
to fix the formatting problems. For that please

1. copy the lines between the long dashes (`—`),
2. store them in a file called `format.patch` **in the root of the repository**

. After that use the following command to apply the changes:

    sh -c '
    line_prefix="$(head -n1 format.patch | sed -nE '"'"'s/(^[0-9]+:).*/\1_/p'"'"' | wc -c | sed -E '"'"'s/[ ]*//g'"'"')"
    { test "$line_prefix" -gt 1 && cut -c"$line_prefix"- format.patch || cat format.patch ; } | patch -p1
    '

.
EOF
)"

git diff --quiet
succeed_if "$error_message"

git diff --quiet || {
	printf '\n\n————————————————————————————————————————————————————————————\n\n'
	git diff -p
	printf '\n\n————————————————————————————————————————————————————————————\n\n'
}

end_script
