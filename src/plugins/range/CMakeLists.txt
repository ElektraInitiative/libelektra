include (LibAddMacros)

if (DEPENDENCY_PHASE AND NOT "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
	set_source_files_properties (range.c PROPERTIES COMPILE_FLAGS -Wno-psabi)
endif ()

add_plugin (range SOURCES range.h range.c LINK_LIBRARIES "-lm" ADD_TEST TEST_README)
