# This functions checks if the dependencies for the YAwn plugin are available.
#
# If they are, the function sets the variable `FOUND_DEPENDENCIES` to `TRUE`. The function then also sets:
#
# - `YAWN_INCLUDE_DIRS` to the path of the include directories needed by YAwn,
# - `YAWN_LIBRARIES` to the path of the libraries needed to compile YAwn, and
# - `SOURCE_FILES` to the list of source files of the YAwn plugin
#
# . If the function was unsuccessful it sets `FOUND_DEPENDENCIES` to `FALSE` and stores the reason for the failure in the variable
# `FAILURE_MESSAGE`.
function (check_dependencies)
	set (FOUND_DEPENDENCIES FALSE PARENT_SCOPE)

	find_package (YAEP QUIET)
	if (NOT YAEP_FOUND)
		set (FAILURE_MESSAGE "YAEP could not be found" PARENT_SCOPE)
		return ()
	endif (NOT YAEP_FOUND)

	include (CheckIncludeFileCXX)
	set (CMAKE_REQUIRED_QUIET ON)
	check_include_file_cxx (codecvt HAVE_CODECVT)
	if (NOT HAVE_CODECVT)
		set (FAILURE_MESSAGE "the current C++ library does not provide header file `codecvt`" PARENT_SCOPE)
		return ()
	endif (NOT HAVE_CODECVT)

	set (YAWN_INCLUDE_DIRS ${YAEP_INCLUDE_DIRS} PARENT_SCOPE)
	set (YAWN_LIBRARIES_CPP ${YAEP_LIBRARIES_CPP} PARENT_SCOPE)

	set (SOURCE_FILES_INPUT input.hpp input.cpp)
	set (SOURCE_FILE_GRAMMAR ${CMAKE_CURRENT_BINARY_DIR}/yaml_grammar.h)

	set (LIBSTDCPP
	     CMAKE_COMPILER_IS_GNUCXX
	     OR
	     ${CMAKE_SYSTEM_NAME}
	     STREQUAL
	     "Linux")
	if (ENABLE_ASAN AND ${LIBSTDCPP})
		# Ignore runtime error about member call on address, which does not point to object of type `__codecvt_abstract_base` in
		# `libstdc++`. See also: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81068
		set_source_files_properties (${SOURCE_FILES_INPUT}
					     PROPERTIES
					     COMPILE_FLAGS
					     "-fno-sanitize=undefined")
	endif (ENABLE_ASAN AND ${LIBSTDCPP})

	# Make sure the build system recreates the header for the grammar, if we modify `yaml.bnf`.
	configure_file (yaml.bnf yaml.bnf @ONLY)
	file (READ ${CMAKE_CURRENT_BINARY_DIR}/yaml.bnf GRAMMAR_INPUT)
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
		set (GRAMMAR "${GRAMMAR}\"${line}\\n\"\n")
	endforeach (line ${GRAMMAR_INPUT})

	file (WRITE ${SOURCE_FILE_GRAMMAR} ${GRAMMAR})

	set (SOURCE_FILES
	     ${SOURCE_FILES_INPUT}
	     ${SOURCE_FILE_GRAMMAR}
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
	     convert.hpp
	     convert.cpp
	     yawn.hpp
	     yawn.cpp
	     PARENT_SCOPE)

	set (FOUND_DEPENDENCIES TRUE PARENT_SCOPE)
endfunction (check_dependencies)

if (DEPENDENCY_PHASE)
	check_dependencies ()
	if (NOT FOUND_DEPENDENCIES)
		remove_plugin (yawn ${FAILURE_MESSAGE})
	endif (NOT FOUND_DEPENDENCIES)
endif (DEPENDENCY_PHASE)

add_plugin (yawn
	    CPP
	    ADD_TEST
	    CPP_TEST
	    INSTALL_TEST_DATA
	    TEST_README
	    TEST_REQUIRED_PLUGINS directoryvalue yamlsmith
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_SYSTEM_DIRECTORIES ${YAWN_INCLUDE_DIRS}
	    LINK_LIBRARIES ${YAWN_LIBRARIES_CPP})
