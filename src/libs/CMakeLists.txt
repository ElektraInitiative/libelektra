include (LibAddLib)

add_subdirectory (loader)

add_subdirectory (tools)

add_subdirectory (ease)

add_subdirectory (proposal)

add_subdirectory (meta)

add_subdirectory (plugin)

add_subdirectory (elektra)

generate_manpage (elektra-libs FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md SECTION 7)
