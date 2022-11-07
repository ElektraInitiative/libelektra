#!/bin/sh

@INCLUDE_COMMON@

echo
echo CHECK SOURCE WITH OCLINT
echo

command -v oclint > /dev/null 2>&1 || {
	echo "Could not locate OCLint" >&2
	exit 0
}

test -f "@PROJECT_BINARY_DIR@/compile_commands.json" || {
	echo "Compilation database not found" >&2
	exit 0
}

set -- \
	"src/libs/ease/array.c" \
	"src/libs/ease/keyname.c" \
	"src/libs/utility/text.c" \
	"src/plugins/base64/"*.c \
	"src/plugins/ccode/"*.cpp \
	"src/plugins/cpptemplate/"*.cpp \
	"src/plugins/directoryvalue/"*.cpp \
	"src/plugins/mini/mini.c"

if [ "$(uname -s)" = Darwin ] && [ "$(uname -r | cut -d '.' -f1)" -eq 19 ]; then
	printerr 'OCLint does not work on macOS 10.15 if source files use headers that were included using the option `SYSTEM`.\n'
	printerr 'The script will therefore *not* check the source files of the YAML CPP and Yan LR plugins.'
else
	set -- $@ \
		"src/plugins/yamlcpp/"*.cpp
fi

cd "@CMAKE_SOURCE_DIR@" || exit
oclint -p "@PROJECT_BINARY_DIR@" -enable-global-analysis -enable-clang-static-analyzer $@

exit_if_fail "OCLint found problematic code"

end_script
