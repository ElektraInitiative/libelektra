include (LibAddMacros)
find_package (LibGit2 0.24.1 QUIET)
if (LibGit2_FOUND)
	set (CMERGE_INCLUDE_DIRS ${LibGit2_INCLUDE_DIRS})
	set (CMERGE_LIBRARY_DIRS ${LibGit2_LIBRARIES})
	add_definitions (-DLIBGITFOUND)
endif ()

# This is derived from LibAddPlugin LibAddLib has no such functionality yet This is required so that libelektra-full.so has defined
# references to the functions from libgit2.so I am not sure if the second set_property from LibAddPlugin has to be here as well
set_property (GLOBAL APPEND PROPERTY "elektra-full_LIBRARIES" ${CMERGE_LIBRARY_DIRS})

# elektra-ease is required for elektraArrayIncName and elektraArrayValidateName see add_plugin for how LINK_ELEKTRA etc. work
#
# we have to link CMERGE_LIBRARY_DIRS here so that elektra-merge.so has the functions from libgit2.so
add_lib (merge
	 SOURCES
	 kdbmerge.c
	 INCLUDE_SYSTEM_DIRECTORIES
	 ${CMERGE_INCLUDE_DIRS}
	 LINK_ELEKTRA
	 elektra-ease
	 LINK_LIBRARIES
	 ${CMERGE_LIBRARY_DIRS})
