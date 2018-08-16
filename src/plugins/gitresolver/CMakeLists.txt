include (LibAddMacros)
if (DEPENDENCY_PHASE)
	find_package (LibGit2 QUIET)
	if (NOT LibGit2_FOUND)
		remove_plugin (gitresolver "Cannot find libgit2-dev")
	endif ()
	if (LibGit2_FOUND)
		try_compile (HAS_LIBGIT2_0.24.1
			     "${CMAKE_BINARY_DIR}"
			     "${PROJECT_SOURCE_DIR}/src/plugins/gitresolver/gitresolver_test.c"
			     CMAKE_FLAGS -DINCLUDE_DIRECTORIES:STRING=${LibGit2_INCLUDE_DIR}
					 -DLINK_LIBRARIES:PATH=${LibGit2_LIBRARIES})
		if (NOT HAS_LIBGIT2_0.24.1)
			remove_plugin (gitresolver "libgit2 >= 0.24.1 required")
		endif ()
	endif ()
	find_package (LibOpenSSL)
	if (NOT OPENSSL_FOUND)
		remove_plugin (gitresolver "OpenSSL-dev not found")
	endif ()
	if (OPENSSL_FOUND AND HAS_LIBGIT2_0.24.1)
		set (GITRESOLVER_INCLUDE_DIRS ${LibGit2_INCLUDE_DIR} ${OPENSSL_INCLUDE_DIRS})
		set (GITRESOLVER_LIBRARY_DIRS ${LibGit2_LIBRARIES} ${OPENSSL_LIBRARIES})
	endif ()

endif ()
add_plugin (gitresolver
	    SOURCES gitresolver.h
		    gitresolver.c
	    LINK_ELEKTRA elektra-invoke
	    INCLUDE_DIRECTORIES ${GITRESOLVER_INCLUDE_DIRS}
	    LINK_LIBRARIES ${GITRESOLVER_LIBRARY_DIRS})
