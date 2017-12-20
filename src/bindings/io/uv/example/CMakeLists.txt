include(LibAddMacros)

file (GLOB HDR_FILES *.h)
file (GLOB SRC_FILES *.c)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

## Build test
set (example exampleio_uv)

add_executable (${example} ${SOURCES})
add_dependencies (${example} kdberrors_generated)

target_include_directories (${example} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

target_link_elektra(${example} elektra-kdb elektra-io elektra-io-uv)

if (LIBUV_VERSION VERSION_LESS "1.0")
	set_target_properties (${example} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV0")
else()
	set_target_properties (${example} PROPERTIES APPEND PROPERTY COMPILE_DEFINITIONS "HAVE_LIBUV1")
endif()
