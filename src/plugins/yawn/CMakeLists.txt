if (DEPENDENCY_PHASE)
	find_package (YAEP QUIET)
	if (NOT YAEP_FOUND)
		remove_plugin (yawn "YAEP could not be found")
	endif (NOT YAEP_FOUND)

	set (SOURCE_FILES_INPUT input.hpp input.cpp)
	set (SOURCE_FILE_GRAMMAR ${CMAKE_CURRENT_BINARY_DIR}/yaml.h)

	set (LIBSTDCPP CMAKE_COMPILER_IS_GNUCXX OR ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	if (ENABLE_ASAN AND ${LIBSTDCPP})
		# Ignore runtime error about member call on address, which does not point to object of type `__codecvt_abstract_base` in
		# `libstdc++`. See also: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81068
		set_source_files_properties (${SOURCE_FILES_INPUT} PROPERTIES COMPILE_FLAGS "-fno-sanitize=undefined")
	endif (ENABLE_ASAN AND ${LIBSTDCPP})

	file (READ yaml.bnf
		   GRAMMAR_INPUT)
	string (REGEX
		REPLACE ";"
			"\\\\\\\\;"
			GRAMMAR_INPUT
			"${GRAMMAR_INPUT}")
	string (REGEX
		REPLACE "\n"
			";"
			GRAMMAR_INPUT
			"${GRAMMAR_INPUT}")
	foreach (line ${GRAMMAR_INPUT})
		string (APPEND GRAMMAR "\"${line}\\n\"\n")
	endforeach (line ${GRAMMAR_INPUT})

	file (WRITE ${SOURCE_FILE_GRAMMAR}
		    ${GRAMMAR})

	set (SOURCE_FILES
	     ${SOURCE_FILES_INPUT}
	     ${SOURCE_FILE_GRAMMAR}
	     location.hpp
	     position.hpp
	     token.hpp
	     token.cpp
	     objstack.h
	     memory.cpp
	     memory.hpp
	     error_listener.hpp
	     error_listener.cpp
	     lexer.hpp
	     lexer.cpp
	     walk.hpp
	     walk.cpp
	     listener.hpp
	     listener.cpp
	     convert.hpp
	     convert.cpp
	     yawn.hpp
	     yawn.cpp)

endif (DEPENDENCY_PHASE)

add_plugin (yawn
	    CPP
	    ADD_TEST
	    CPP_TEST
	    INSTALL_TEST_DATA
	    TEST_README
	    TEST_REQUIRED_PLUGINS directoryvalue
				  yamlsmith
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${YAEP_INCLUDE_DIR}
	    LINK_LIBRARIES ${YAEP_LIBRARIES_CPP})
