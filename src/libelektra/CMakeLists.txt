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

file (GLOB SRC_FILES *.c)
file (GLOB HDR_FILES ${PROJECT_BINARY_DIR}/src/include/*.h)

#libhelper
set (SRC_FILES ${SRC_FILES} "../libhelper/internal.c")


#libloader
set (SRC_FILES ${SRC_FILES} "../libloader/dl.c")
find_package (DL REQUIRED)

include_directories (${DL_INCLUDE_DIRS})
include_directories ("${PROJECT_BINARY_DIR}/src/include")

set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektra SHARED ${SOURCES})
target_link_libraries (elektra ${DL_LIBRARIES})
set_target_properties (elektra PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)
set_target_properties (elektra PROPERTIES
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION})

install_targets (/lib elektra)
