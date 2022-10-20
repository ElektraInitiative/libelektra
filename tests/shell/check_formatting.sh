#!/bin/sh

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
	reformat_command_output="$(scripts/dev/$reformat_command 2>&1)" || {
		printf >&2 -- '————————————————————————————————————————————————————————————\n'
		printf >&2 -- 'Warning — Reformatting command `%s` failed\n' "$reformat_command"
		printf >&2 -- '\n%s\n' "$reformat_command_output"
		printf >&2 -- '————————————————————————————————————————————————————————————\n\n'
	}
}

reformat reformat-c &
reformat reformat-cmake &
reformat reformat-java &
reformat reformat-javascript &
reformat reformat-markdown &
reformat reformat-shell &
wait

error_message="$(
	cat << 'EOF'
The reformatting check detected code that **does not** fit the guidelines given in `doc/CODING.md`.
If you see this message on one of the build servers, you can either install one or multiple of the following tools:

- [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) to format C and C++ source code,
- [`cmake_format`](https://github.com/cheshirekow/cmake_format) to format CMake code,
- [`prettier`](https://prettier.io) to format JavaScript & Markdown code, and
- [`shfmt`](https://github.com/mvdan/sh) to format Shell code
- [`google-java-format`](https://google.github.io/styleguide/javaguide.html) to format Java source code

. Afterwards you can use the following scripts to fix the formatting problems

- `reformat-c` to format C/C++ source files,
- `reformat-cmake` to format CMake files,
- `reformat-java` to format Java files,
- `reformat-javascript` to format JavaScript files,
- `reformat-markdown` to format Markdown files, and
- `reformat-shell` to format files that contain shell code

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

git_diff_output="$(git diff -p 2>&1)"

if [ $? -ne 0 ]; then
	error_message="$(printf 'Unable to create diff: %s' "$git_diff_output" 2>&1)"
	false
	exit_if_fail "$error_message"
fi

if [ -n "$git_diff_output" ]; then
	false
	succeed_if "$error_message"
	printf '\n\n————————————————————————————————————————————————————————————\n\n'
	printf '%s' "$git_diff_output"
	printf '\n\n————————————————————————————————————————————————————————————\n\n'

fi

# We stash working directory and staged changes. This operation should only remove formatting modifications, since we already checked for a
# clean state at the beginning of this script. We stash all file changes, since otherwise the test will always succeed on the second
# invocation. See also: http://issues.libelektra.org/3281
git stash push -m 'Formatting Updates'

end_script
