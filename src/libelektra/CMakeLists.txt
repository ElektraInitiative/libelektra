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

file (GLOB SRC_FILES *.c)

#libhelper
set (SRC_FILES ${SRC_FILES} "../libhelper/internal.c")


#libloader
set (SRC_FILES ${SRC_FILES} "../libloader/dl.c")
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
find_package (DL REQUIRED)

include_directories (${DL_INCLUDE_DIRS})
include_directories ("${PROJECT_BINARY_DIR}/src/include")

add_library (elektra SHARED ${SRC_FILES})
target_link_libraries (elektra ${DL_LIBRARIES})
set_target_properties (elektra PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)

install_targets (/lib elektra)
