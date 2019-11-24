include (LibAddMacros)

if (DEPENDENCY_PHASE)
	include (CheckIncludeFile)
	set (CMAKE_REQUIRED_QUIET ON)
	check_include_file (fts.h HAVE_FTS_H)

	if (NOT HAVE_FTS_H)
		remove_plugin (multifile "header file “fts.h” was not found")
	endif (NOT HAVE_FTS_H)
endif (DEPENDENCY_PHASE)

add_plugin (
	multifile
	SOURCES multifile.h multifile.c
	LINK_ELEKTRA elektra-kdb elektra-invoke
	TEST_README)
