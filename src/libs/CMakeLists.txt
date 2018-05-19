include (LibAddLib)

add_subdirectory (loader)

add_subdirectory (tools)

add_subdirectory (ease)

add_subdirectory (proposal)

add_subdirectory (meta)

add_subdirectory (plugin)

add_subdirectory (pluginprocess)

add_subdirectory (utility)

add_subdirectory (io)

add_subdirectory (invoke)

add_subdirectory (notification)

add_subdirectory (elektra)

add_subdirectory (typesystem/libfa)

add_subdirectory (typesystem/specelektra)

add_subdirectory (typesystem/spectranslator)

generate_manpage (elektra-libs FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md SECTION 7)
