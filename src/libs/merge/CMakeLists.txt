include (LibAddMacros)
find_package (LibGit2 0.24.1 QUIET)
if (LibGit2_FOUND)
	set (CMERGE_INCLUDE_DIRS ${LibGit2_INCLUDE_DIRS})
	set (CMERGE_LIBRARY_DIRS ${LibGit2_LIBRARIES})
	add_definitions (-DLIBGITFOUND)
endif ()
add_lib (merge
	 SOURCES
	 kdbmerge.c
	 INCLUDE_SYSTEM_DIRECTORIES
	 ${CMERGE_INCLUDE_DIRS}
	 LINK_LIBRARIES
	 elektra-ease
	 ${CMERGE_LIBRARY_DIRS})
