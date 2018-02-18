include (LibAddMacros)

file (GLOB HDR_FILES *.h)
file (GLOB SRC_FILES *.c)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

## Build test
set (example example_notification)

add_executable (${example} ${SOURCES})
add_dependencies (${example} kdberrors_generated)

# target_include_directories (${example} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

target_link_elektra (${example} elektra-kdb elektra-notification)
