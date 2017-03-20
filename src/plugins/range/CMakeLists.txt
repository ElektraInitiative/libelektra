include (LibAddMacros)

if (DEPENDENCY_PHASE)
    set_source_files_properties(range.c PROPERTIES COMPILE_FLAGS -Wno-psabi)
endif ()

add_plugin (range
	SOURCES
		range.h
		range.c
	LINK_LIBRARIES
		"-lm"
	ADD_TEST
	)
