include (LibAddMacros)

if (DEPENDENCY_PHASE)
    set_source_files_properties(range.c PROPERTIES COMPILE_FLAGS -Wno-psabi)
endif ()
if(UNIX)
    link_libraries(m)
endif(UNIX)

add_plugin (range
	SOURCES
		range.h
		range.c
	ADD_TEST
	)
