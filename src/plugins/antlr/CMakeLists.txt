include (LibAddMacros)

if (DEPENDENCY_PHASE)
	execute_process (COMMAND antlr4 RESULT_VARIABLE ANTLR_NOT_AVAILABLE OUTPUT_QUIET)
	if (ANTLR_NOT_AVAILABLE)
		remove_plugin (antlr "ANTLR 4 executable (antlr4) not found")
	endif (ANTLR_NOT_AVAILABLE)

	find_package (ANTLR4CPP QUIET)
	if (NOT ANTLR4CPP_FOUND)
		remove_plugin (antlr "ANTLR 4 CPP runtime (antlr4-cpp-runtime) not found")
	endif (NOT ANTLR4CPP_FOUND)

	# Disable warnings caused by ANTLR’s source code
	if (APPLE AND CMAKE_COMPILER_IS_GNUCXX)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-attributes")
		string (REPLACE "-Woverloaded-virtual" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
		string (REPLACE "-Wshadow" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
	endif (APPLE AND CMAKE_COMPILER_IS_GNUCXX)

	set (GRAMMAR_NAME Test)
	set (GRAMMAR_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${GRAMMAR_NAME}.g4)
	set (GENERATED_SOURCE_FILES
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}Lexer.cpp
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}Parser.cpp
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}BaseListener.cpp
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}BaseVisitor.cpp
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}Listener.cpp
		${CMAKE_CURRENT_BINARY_DIR}/${GRAMMAR_NAME}Visitor.cpp
	)
	set (SOURCE_FILES "${GENERATED_SOURCE_FILES}" antlr.hpp antlr.cpp)
	foreach (file ${GENERATED_SOURCE_FILES})
	      set_source_files_properties (${file} PROPERTIES GENERATED TRUE)
	endforeach (file ${GENERATED_SOURCE_FILES})

	add_custom_command (
		OUTPUT ${GENERATED_SOURCE_FILES}
		COMMAND antlr4 -Werror -Dlanguage=Cpp -listener -visitor -o ${CMAKE_CURRENT_BINARY_DIR} -package antlr ${GRAMMAR_FILE}
		DEPENDS ${GRAMMAR_FILE}
	)
endif (DEPENDENCY_PHASE)

add_plugin (antlr
	CPP
	SOURCES
		${SOURCE_FILES}
	INCLUDE_DIRECTORIES
		${ANTLR4CPP_INCLUDE_DIRS}
	LINK_LIBRARIES
		${ANTLR4CPP_LIBRARIES}
	ADD_TEST
	TEST_README
	)
