# This functions checks if the dependencies for the YAMBi plugin are available.
#
# If they are, the function sets the variable `FOUND_DEPENDENCIES` to `TRUE`. The function then also sets:
#
# - `BISON_VERSION` to the version number of the detected version of Bison
#
# and defines the macro `bison_target`. If the function was unsuccessful it sets `FOUND_DEPENDENCIES` to `FALSE` and stores the reason for
# the failure in the variable `FAILURE_MESSAGE`.
function (check_dependencies)
	set (FOUND_DEPENDENCIES FALSE PARENT_SCOPE)

	include (CheckIncludeFileCXX)
	set (CMAKE_REQUIRED_QUIET ON)
	check_include_file_cxx (codecvt HAVE_CODECVT)
	if (NOT HAVE_CODECVT)
		set (FAILURE_MESSAGE "because the current C++ library does not provide header file `codecvt`" PARENT_SCOPE)
		return ()
	endif (NOT HAVE_CODECVT)

	# Add path to Homebrew version of Bison, if it exists.
	set (USE_HOMEBREW_BISON
	     ${APPLE}
	     AND
	     EXISTS
	     /usr/local/opt/bison)
	if (USE_HOMEBREW_BISON)
		list (APPEND CMAKE_PREFIX_PATH "/usr/local/opt/bison")
	endif (USE_HOMEBREW_BISON)
	find_package (BISON 3 QUIET)

	if (NOT BISON_FOUND)
		set (FAILURE_MESSAGE "Bison 3 (bison) not found" PARENT_SCOPE)
		return ()
	endif (NOT BISON_FOUND)

	if (BISON_VERSION VERSION_LESS 3)
		set (FAILURE_MESSAGE "Bison version 3 or later required (found version ${BISON_VERSION})" PARENT_SCOPE)
		return ()
	endif (BISON_VERSION VERSION_LESS 3)

	set (BISON_VERSION "${BISON_VERSION}" PARENT_SCOPE)
	set (FOUND_DEPENDENCIES TRUE PARENT_SCOPE)

endfunction (check_dependencies)

# This functions generates the source files of the YAML parser using Bison. If generating the source files was successful, then it sets the
# variables:
#
# - `GENERATED_CODE` to `TRUE`,
# - `BISON_YAMBI_OUTPUT_SOURCE` to the filepath of the parser source, and
# - `BISON_YAMBI_OUTPUT_HEADER` to the filepath of the parser header
#
# . If the function was unsuccessful it sets `GENERATED_CODE` to `FALSE` and stores the reason for the error in the variable
# `FAILURE_MESSAGE`.
function (generate_code)

	set (GENERATED_CODE FALSE PARENT_SCOPE)

	if (BISON_VERSION VERSION_LESS 3.3)
		set (PARSER_NAME_DIRECTIVE "parser_class_name")
	else (BISON_VERSION VERSION_LESS 3.3)
		set (PARSER_NAME_DIRECTIVE "api.parser.class")
	endif (BISON_VERSION VERSION_LESS 3.3)

	configure_file (${CMAKE_CURRENT_SOURCE_DIR}/parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.ypp @ONLY)

	bison_target (YAMBI ${CMAKE_CURRENT_BINARY_DIR}/parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp)

	if (NOT BISON_YAMBI_DEFINED)
		set (FAILURE_MESSAGE "generating the parser code failed" PARENT_SCOPE)
		return ()
	endif (NOT BISON_YAMBI_DEFINED)

	set (GENERATED_CODE TRUE PARENT_SCOPE)
	set (BISON_YAMBI_OUTPUT_SOURCE ${BISON_YAMBI_OUTPUT_SOURCE} PARENT_SCOPE)
	set (BISON_YAMBI_OUTPUT_HEADER ${BISON_YAMBI_OUTPUT_HEADER} PARENT_SCOPE)

endfunction (generate_code)

if (DEPENDENCY_PHASE)
	check_dependencies ()
	if (NOT FOUND_DEPENDENCIES)
		remove_plugin (yambi ${FAILURE_MESSAGE})
	else (NOT FOUND_DEPENDENCIES)
		generate_code ("${BISON_VERSION}")
		if (NOT GENERATED_CODE)
			remove_plugin (yambi ${FAILURE_MESSAGE})
		endif (NOT GENERATED_CODE)
	endif (NOT FOUND_DEPENDENCIES)

	set (SOURCE_FILES_INPUT input.hpp input.cpp)

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

	set (SOURCE_FILES
	     convert.hpp
	     convert.cpp
	     driver.hpp
	     driver.cpp
	     lexer.hpp
	     lexer.cpp
	     yambi.hpp
	     yambi.cpp
	     ${SOURCE_FILES_INPUT}
	     ${BISON_YAMBI_OUTPUT_SOURCE}
	     ${BISON_YAMBI_OUTPUT_HEADER})
endif (DEPENDENCY_PHASE)

add_plugin (yambi
	    CPP
	    ADD_TEST
	    CPP_TEST
	    INSTALL_TEST_DATA
	    TEST_README
	    TEST_REQUIRED_PLUGINS directoryvalue yamlsmith
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR})
