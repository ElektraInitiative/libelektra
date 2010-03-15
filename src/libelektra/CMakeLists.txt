add_executable(exportsymbols exportsymbols.c)

get_target_property(exportsymbols EXE_LOC LOCATION)


add_custom_command(
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.h
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS BACKENDS)

add_custom_command(
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.c
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS BACKENDS)

file(GLOB SRC_FILES *.c)
include_directories("${PROJECT_BINARY_DIR}/src/include")

add_library(elektra SHARED ${SRC_FILES})
set_target_properties (elektra PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)
