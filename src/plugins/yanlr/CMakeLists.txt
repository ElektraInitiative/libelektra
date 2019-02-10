if (DEPENDENCY_PHASE)
	execute_process (COMMAND antlr4 RESULT_VARIABLE ANTLR4_NOT_AVAILABLE OUTPUT_QUIET)
	execute_process (COMMAND antlr RESULT_VARIABLE ANTLR_NOT_AVAILABLE OUTPUT_QUIET)
	if (ANTLR4_NOT_AVAILABLE AND ANTLR_NOT_AVAILABLE)
		remove_plugin (yanlr "ANTLR 4 executable (antlr4, antlr) not found")
	else (ANTLR4_NOT_AVAILABLE AND ANTLR_NOT_AVAILABLE)
		if (ANTLR4_NOT_AVAILABLE)
			set (ANTLR_EXECUTABLE antlr)
		else (ANTLR4_NOT_AVAILABLE)
			set (ANTLR_EXECUTABLE antlr4)
		endif (ANTLR4_NOT_AVAILABLE)
	endif (ANTLR4_NOT_AVAILABLE AND ANTLR_NOT_AVAILABLE)

	find_package (ANTLR4CPP QUIET)
	if (NOT ANTLR4CPP_FOUND)
		remove_plugin (yanlr "ANTLR 4 CPP runtime (antlr4-cpp-runtime) not found")
	endif (NOT ANTLR4CPP_FOUND)

	set (DISABLE_PLUGIN_ASAN
	     ${ENABLE_ASAN}
	     AND
	     "${CMAKE_CXX_COMPILER_ID}"
	     MATCHES
	     "GNU"
	     AND
	     ${CMAKE_CXX_COMPILER_VERSION}
	     VERSION_LESS
	     7)
	if (${DISABLE_PLUGIN_ASAN})
		remove_plugin (yanlr "ASAN enabled GCC builds of the plugin report memory leaks")
	endif (${DISABLE_PLUGIN_ASAN})

	set (GRAMMAR_NAME YAML)
	set (GRAMMAR_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${GRAMMAR_NAME}.g4)
	set (GENERATED_SOURCE_FILES_NAMES BaseListener Listener)
	foreach (file ${GENERATED_SOURCE_FILES_NAMES} "")
		set (filepath ${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}${file}.cpp)
		set_source_files_properties (${filepath} PROPERTIES GENERATED TRUE)
		list (APPEND GENERATED_SOURCE_FILES
			     ${filepath})
	endforeach (file ${GENERATED_SOURCE_FILES_NAMES})

	set (SOURCE_FILES
	     "${GENERATED_SOURCE_FILES}"
	     listener.hpp
	     listener.cpp
	     error_listener.hpp
	     error_listener.cpp
	     yaml_lexer.hpp
	     yaml_lexer.cpp
	     yanlr.hpp
	     yanlr.cpp)

	add_custom_command (OUTPUT ${GENERATED_SOURCE_FILES}
			    COMMAND ${ANTLR_EXECUTABLE} -Werror -Dlanguage=Cpp -o ${CMAKE_CURRENT_BINARY_DIR} -package antlr ${GRAMMAR_FILE}
			    DEPENDS ${GRAMMAR_FILE}
			    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif (DEPENDENCY_PHASE)

# ~~~
# The generated code seems to contain a
# [double free](https://build.libelektra.org/jenkins/blue/organizations/jenkins/libelektra/detail/PR-2271/155/pipeline) that causes the
# unit test to crash with a segfault on systems that use `glibc`.
# ~~~
set (DISABLE_UNIT_TEST "${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU" AND ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS 7)

if (${DISABLE_UNIT_TEST})
	set (TEST_ARGUMENTS "")
else (${DISABLE_UNIT_TEST})
	set (TEST_ARGUMENTS ADD_TEST CPP_TEST)
endif (${DISABLE_UNIT_TEST})

add_plugin (yanlr
	    CPP
	    ${TEST_ARGUMENTS}
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_SYSTEM_DIRECTORIES ${ANTLR4CPP_INCLUDE_DIRS}
	    LINK_LIBRARIES ${ANTLR4CPP_LIBRARIES}
	    LINK_ELEKTRA elektra-ease
	    INSTALL_TEST_DATA
	    TEST_README
	    # Unfortunately it looks like ANTLR’s code [causes a container-overflow](https://github.com/antlr/antlr4/issues/2332).
	    TEST_ENVIRONMENT "ASAN_OPTIONS=detect_container_overflow=0"
	    TEST_REQUIRED_PLUGINS directoryvalue
				  yamlsmith)
