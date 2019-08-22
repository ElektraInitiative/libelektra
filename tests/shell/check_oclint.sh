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

cd "@CMAKE_SOURCE_DIR@" || exit
oclint -p "@PROJECT_BINARY_DIR@" -no-analytics -enable-global-analysis -enable-clang-static-analyzer \
	"src/libs/ease/array.c" \
	"src/libs/ease/keyname.c" \
	"src/libs/utility/text.c" \
	"src/plugins/base64/"*.c \
	"src/plugins/camel/camel.c" \
	"src/plugins/ccode/"*.cpp \
	"src/plugins/cpptemplate/"*.cpp \
	"src/plugins/directoryvalue/"*.cpp \
	"src/plugins/mini/mini.c" \
	"src/plugins/yambi/"*.cpp \
	"src/plugins/yamlcpp/"*.{c,cpp} \
	"src/plugins/yamlsmith/"*.{c,cpp} \
	"src/plugins/yanlr/"*.cpp \
	"src/plugins/yawn/"*.cpp
exit_if_fail "OCLint found problematic code"

end_script
