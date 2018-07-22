if (DEPENDENCY_PHASE)
	execute_process (COMMAND antlr4 RESULT_VARIABLE ANTLR_NOT_AVAILABLE OUTPUT_QUIET)
	if (ANTLR_NOT_AVAILABLE)
		remove_plugin (yanlr "ANTLR 4 executable (antlr4) not found")
	endif (ANTLR_NOT_AVAILABLE)

	find_package (ANTLR4CPP QUIET)
	if (NOT ANTLR4CPP_FOUND)
		remove_plugin (yanlr "ANTLR 4 CPP runtime (antlr4-cpp-runtime) not found")
	endif (NOT ANTLR4CPP_FOUND)

	# Disable warnings caused by ANTLR’s source code
	if (APPLE AND CMAKE_COMPILER_IS_GNUCXX)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-attributes")
		string (REPLACE "-Woverloaded-virtual"
				""
				CMAKE_CXX_FLAGS
				"${CMAKE_CXX_FLAGS}")
		string (REPLACE "-Wshadow"
				""
				CMAKE_CXX_FLAGS
				"${CMAKE_CXX_FLAGS}")
	endif (APPLE AND CMAKE_COMPILER_IS_GNUCXX)

	set (GRAMMAR_NAME YAML)
	set (GRAMMAR_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${GRAMMAR_NAME}.g4)
	set (GENERATED_SOURCE_FILES_NAMES BaseListener Listener)
	foreach (file ${GENERATED_SOURCE_FILES_NAMES} "")
		set (filepath ${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}${file}.cpp)
		set_source_files_properties (${filepath} PROPERTIES GENERATED TRUE)
		list (APPEND GENERATED_SOURCE_FILES
			     ${filepath})
	endforeach (file ${GENERATED_SOURCE_FILES_NAMES})

	set (SOURCE_FILES "${GENERATED_SOURCE_FILES}" listener.hpp listener.cpp yaml_lexer.hpp yaml_lexer.cpp yanlr.hpp yanlr.cpp)

	add_custom_command (OUTPUT ${GENERATED_SOURCE_FILES}
			    COMMAND antlr4 -Werror -Dlanguage=Cpp -o ${CMAKE_CURRENT_BINARY_DIR} -package antlr ${GRAMMAR_FILE}
			    DEPENDS ${GRAMMAR_FILE}
			    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif (DEPENDENCY_PHASE)

add_plugin (yanlr
	    CPP
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${ANTLR4CPP_INCLUDE_DIRS}
	    LINK_LIBRARIES ${ANTLR4CPP_LIBRARIES}
	    LINK_ELEKTRA elektra-ease
	    ADD_TEST
	    TEST_README
	    TEST_ENVIRONMENT "ASAN_OPTIONS=detect_container_overflow=0" # Unfortunately it looks like ANTLR’s code causes a container-
									# overflow
	    TEST_REQUIRED_PLUGINS yamlsmith)
