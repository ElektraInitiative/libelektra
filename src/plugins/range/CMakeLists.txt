include (LibAddMacros)

if (DEPENDENCY_PHASE AND NOT "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
	set_source_files_properties (range.c PROPERTIES COMPILE_FLAGS -Wno-psabi)
endif ()

add_plugin (
	range
	SOURCES range.h range.c
	LINK_LIBRARIES m
	ADD_TEST TEST_README COMPONENT libelektra${SO_VERSION}-experimental)

if (ADDTESTING_PHASE)
	if (CMAKE_C_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

		# Disable test for Clang + valgrind combination: https://github.com/ElektraInitiative/libelektra/issues/1871
		set_property (TEST "testmod_range" PROPERTY LABELS memleak)
	endif (CMAKE_C_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
endif (ADDTESTING_PHASE)
