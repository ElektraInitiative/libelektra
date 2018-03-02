include (LibAddMacros)

file (GLOB HDR_FILES *.h)
file (GLOB SRC_FILES *.c)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

if (BUILD_FULL OR BUILD_STATIC)
	# add sources for elektra-io-uv for static and full builds
	list (APPEND SOURCES $<TARGET_OBJECTS:OBJ_elektra-io-uv>)
endif ()

## Build test
set (example exampleio_uv)

add_executable (${example} ${SOURCES})
add_dependencies (${example} kdberrors_generated)

target_include_directories (${example} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

target_link_elektra (${example} elektra-kdb elektra-io elektra-io-uv)
if (BUILD_FULL OR BUILD_STATIC)
	target_link_libraries (${example} ${LIBUV_LDFLAGS})
endif ()
