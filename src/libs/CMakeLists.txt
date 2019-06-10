include (LibAddLib)

add_subdirectory (loader)

add_subdirectory (tools)

add_subdirectory (ease)

add_subdirectory (globbing)

add_subdirectory (proposal)

add_subdirectory (meta)

add_subdirectory (plugin)

add_subdirectory (pluginprocess)

add_subdirectory (utility)

add_subdirectory (io)

add_subdirectory (invoke)

add_subdirectory (notification)

add_subdirectory (highlevel)

add_subdirectory (opts)

add_subdirectory (merge)

# The subdirectory for LibElektra must be the last entry!
add_subdirectory (elektra)

generate_manpage (elektra-libs
		  FILENAME
		  ${CMAKE_CURRENT_SOURCE_DIR}/README.md
		  SECTION
		  7)
