include (LibAddMacros)
#if (DEPENDENCY_PHASE)
    #    find_package(LibGit2)
    #    if (NOT LibGit2_FOUND)
        #        remove_plugin (gitresolver "Cannot find libgit2-dev")
        #    endif ()
    #endif ()
set (CMAKE_SHARED_LINKER_FLAGS "-lgit2")
add_plugin (gitresolver
	SOURCES
		gitresolver.h
		gitresolver.c
        #    INCLUDE_DIRECTORIES
    #    ${LibGit2_INCLUDE_DIR}
    #    LINK_LIBRARIES
    #    ${LibGit2_LIBRARIES}
    LINK_LIBRARIES
        "git2"
        ADD_TEST
	)
