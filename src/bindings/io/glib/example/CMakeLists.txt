include (LibAddMacros)

file (GLOB HDR_FILES *.h)
file (GLOB SRC_FILES *.c)

set (SOURCES ${SRC_FILES} ${HDR_FILES} ${ELEKTRA_HEADERS})

if (BUILD_FULL OR BUILD_STATIC)
	list (APPEND SOURCES $<TARGET_OBJECTS:elektra-io-glib-objects>) # add sources for elektra-io-glib for static and full builds
endif ()

# Build test
set (example exampleio_glib)

add_executable (${example} ${SOURCES})

target_include_directories (${example} SYSTEM PRIVATE ${GLib_INCLUDE_DIRS})

target_link_elektra (${example} elektra-kdb elektra-io elektra-io-glib)
if (BUILD_FULL OR BUILD_STATIC)
	target_link_libraries (${example} ${GLib_LIBRARY})
endif ()
