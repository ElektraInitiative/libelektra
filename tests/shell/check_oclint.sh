@INCLUDE_COMMON@

echo
echo CHECK SOURCE WITH OCLINT
echo

command -v oclint >/dev/null 2>&1 || { echo "Could not locate OCLint" >&2; exit 0; }

test -f "@PROJECT_BINARY_DIR@/compile_commands.json" || { echo "Compilation database not found" >&2; exit 0; }

cd "@CMAKE_SOURCE_DIR@" || exit
oclint -p "@PROJECT_BINARY_DIR@" -enable-global-analysis -enable-clang-static-analyzer \
	"@CMAKE_SOURCE_DIR@/src/libs/ease/array.c" \
	"@CMAKE_SOURCE_DIR@/src/libs/ease/keyname.c" \
	"@CMAKE_SOURCE_DIR@/src/libs/utility/text.c" \
	"@CMAKE_SOURCE_DIR@/src/plugins/base64/"*.c \
	"@CMAKE_SOURCE_DIR@/src/plugins/ccode/"*.cpp \
	"@CMAKE_SOURCE_DIR@/src/plugins/camel/camel.c" \
	"@CMAKE_SOURCE_DIR@/src/plugins/directoryvalue/"*.c \
	"@CMAKE_SOURCE_DIR@/src/plugins/mini/mini.c" \
	"@CMAKE_SOURCE_DIR@/src/plugins/yamlcpp/"*.{c,cpp}
exit_if_fail "OCLint found problematic code"

end_script
