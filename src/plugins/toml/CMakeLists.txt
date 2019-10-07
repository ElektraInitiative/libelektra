
if (DEPENDENCY_PHASE)

    set (SOURCE_FILES
        toml.h
        toml.c)

endif (DEPENDENCY_PHASE)


add_plugin (toml
    C
    SOURCES ${SOURCE_FILES}
    INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR})
