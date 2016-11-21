add_test (
	testshell_enum
	"${CMAKE_CURRENT_SOURCE_DIR}/markdown_shell_recorder.sh"
	"${CMAKE_SOURCE_DIR}/src/plugins/enum/README.md"
	)

set_property(TEST testshell_enum PROPERTY LABELS memleak kdbtests)
