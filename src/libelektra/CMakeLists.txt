add_executable(exportsymbols exportsymbols.c)

get_target_property(exportsymbols EXE_LOC LOCATION)

set(BACKENDS a b c)

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

file(GLOB src_files *.c ../include/*.h)

add_library(elektra ${src_files})
