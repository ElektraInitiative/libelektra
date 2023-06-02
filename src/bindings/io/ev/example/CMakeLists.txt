include (LibAddMacros)

file (GLOB HDR_FILES *.h)
file (GLOB SRC_FILES *.c)

add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

if (BUILD_FULL OR BUILD_STATIC) # add sources for elektra-io-ev for static and full builds
	list (APPEND SOURCES $<TARGET_OBJECTS:elektra-io-ev-objects>)
endif ()

# Build test
set (example exampleio_ev)

add_executable (${example} ${SOURCES})

target_include_directories (${example} SYSTEM PRIVATE ${LIBEV_INCLUDE_DIRS})

target_link_elektra (${example} elektra-kdb elektra-io elektra-io-ev)
if (BUILD_FULL OR BUILD_STATIC)
	target_link_libraries (${example} ${LIBEV_LIBRARIES})
endif ()
