if (DEPENDENCY_PHASE)
	find_package (YAEP QUIET)
	if (NOT YAEP_FOUND)
		remove_plugin (yawn "YAEP could not be found")
	endif (NOT YAEP_FOUND)

	set (SOURCE_FILES_INPUT input.hpp input.cpp)
	set (SOURCE_FILES_MEMORY objstack.h memory.cpp memory.hpp convert.hpp convert.cpp)
	set_source_files_properties (${SOURCE_FILES_MEMORY} PROPERTIES COMPILE_FLAGS "-Wno-old-style-cast")

	set (LIBSTDCPP CMAKE_COMPILER_IS_GNUCXX OR ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	if (ENABLE_ASAN AND ${LIBSTDCPP})
		# Ignore runtime error about member call on address, which does not point to object of type `__codecvt_abstract_base` in
		# `libstdc++`. See also: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81068
		set_source_files_properties (${SOURCE_FILES_INPUT} PROPERTIES COMPILE_FLAGS "-fno-sanitize=undefined")
	endif (ENABLE_ASAN AND ${LIBSTDCPP})

	set (SOURCE_FILES
	     ${SOURCE_FILES_INPUT}
	     ${SOURCE_FILES_MEMORY}
	     location.hpp
	     position.hpp
	     token.hpp
	     token.cpp
	     error_listener.hpp
	     error_listener.cpp
	     lexer.hpp
	     lexer.cpp
	     walk.hpp
	     walk.cpp
	     listener.hpp
	     listener.cpp
	     yawn.hpp
	     yawn.cpp)

endif (DEPENDENCY_PHASE)

add_plugin (yawn
	    CPP
	    ADD_TEST
	    CPP_TEST
	    TEST_README
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${YAEP_INCLUDE_DIR}
	    LINK_LIBRARIES ${YAEP_LIBRARIES_CPP})
