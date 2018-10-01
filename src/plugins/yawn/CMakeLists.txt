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

	set (OBJSTACK_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/yaep)
	set (OBJSTACK_FILE ${OBJSTACK_INCLUDE_DIR}/objstack.h)
	set (OBJSTACK_HASH_EXPECTED_PART_ONE b9d237e955ff450f8adfacd3048999d96dd3738275e8273fb9fc8312187089cb)
	set (OBJSTACK_HASH_EXPECTED_PART_TWO 1573ffaf4f6c5ba36c1a2f1b4885dbae9754f5f13131c8d45664d203d33b0c7f)
	set (OBJSTACK_HASH_EXPECTED "${OBJSTACK_HASH_EXPECTED_PART_ONE}${OBJSTACK_HASH_EXPECTED_PART_TWO}")

	file (DOWNLOAD "https://raw.githubusercontent.com/vnmakarov/yaep/a1b930600b8bd7cd98ad2f4784d5f3272a853eeb/src/objstack.h"
		       ${OBJSTACK_FILE}
	      STATUS OBJSTACK_DOWNLOAD_INFO)

	list (GET OBJSTACK_DOWNLOAD_INFO
		  0
		  OBJSTACK_DOWNLOADED_STATUS)
	list (GET OBJSTACK_DOWNLOAD_INFO
		  1
		  OBJSTACK_DOWNLOADED_MESSAGE)

	if (NOT ${OBJSTACK_DOWNLOADED_STATUS} EQUAL 0)
		set (FAILURE_MESSAGE
		     "downloading `objstack.h` from https://github.com/vnmakarov/yaep failed: ${OBJSTACK_DOWNLOADED_MESSAGE}"
		     PARENT_SCOPE)
		return ()
	endif (NOT ${OBJSTACK_DOWNLOADED_STATUS} EQUAL 0)

	file (SHA3_512 ${OBJSTACK_FILE} OBJSTACK_HASH)
	if (NOT OBJSTACK_HASH STREQUAL OBJSTACK_HASH_EXPECTED)
		set (FAILURE_MESSAGE
		     "the SHA3_512 hash for the file `${OBJSTACK_FILE}` “${OBJSTACK_HASH}” does not match “${OBJSTACK_HASH_EXPECTED}”"
		     PARENT_SCOPE)
		return ()
	endif (NOT OBJSTACK_HASH STREQUAL OBJSTACK_HASH_EXPECTED)

	set (YAWN_INCLUDE_DIRS ${YAEP_INCLUDE_DIRS} ${OBJSTACK_INCLUDE_DIR} PARENT_SCOPE)
	set (YAWN_LIBRARIES_CPP ${YAEP_LIBRARIES_CPP} PARENT_SCOPE)

	set (SOURCE_FILES_INPUT input.hpp input.cpp)
	set (SOURCE_FILE_GRAMMAR ${CMAKE_CURRENT_BINARY_DIR}/yaml_grammar.h)

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
		set (GRAMMAR "${GRAMMAR}\"${line}\\n\"\n")
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
	    TEST_REQUIRED_PLUGINS directoryvalue
				  yamlsmith
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_SYSTEM_DIRECTORIES ${YAWN_INCLUDE_DIRS}
	    LINK_LIBRARIES ${YAWN_LIBRARIES_CPP})
