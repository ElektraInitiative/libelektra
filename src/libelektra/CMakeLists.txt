add_executable (exportsymbols exportsymbols.c)

get_target_property (exportsymbols EXE_LOC LOCATION)


add_custom_command (
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.h
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS BACKENDS)

add_custom_command (
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.c
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS BACKENDS)

include(LibAddMacros)
add_headers(HDR_FILES)

file (GLOB SRC_FILES *.c)
list (APPEND SRC_FILES "../libhelper/internal.c")

find_package (DL REQUIRED)
if (DL_FOUND)
	list (APPEND SRC_FILES "../libloader/dl.c")
	include_directories (${DL_INCLUDE_DIRS})
else (DL_FOUND)
	message (ERROR "no dl found")
endif (DL_FOUND)

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektra SHARED ${SOURCES})
target_link_libraries (elektra ${DL_LIBRARIES})
set_target_properties (elektra PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)
set_target_properties (elektra PROPERTIES
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION})

install_targets (/lib elektra)
