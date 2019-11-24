function (check_dependencies)
	set (FOUND_DEPENDENCIES FALSE PARENT_SCOPE)

	find_package (FLEX QUIET)
	find_package (BISON QUIET)

	if ((NOT FLEX_FOUND) AND (NOT BISON_FOUND))
		set (FAILURE_MESSAGE "Flex and Bison could not be found" PARENT_SCOPE)
		return ()
	endif ((NOT FLEX_FOUND) AND (NOT BISON_FOUND))
	if (NOT FLEX_FOUND)
		set (FAILURE_MESSAGE "Flex could not be found" PARENT_SCOPE)
		return ()
	endif (NOT FLEX_FOUND)
	if (NOT BISON_FOUND)
		set (FAILURE_MESSAGE "Bison could not be found" PARENT_SCOPE)
		return ()
	endif (NOT BISON_FOUND)

	set (FOUND_DEPENDENCIES TRUE PARENT_SCOPE)
endfunction (check_dependencies)

function (generate_code)
	set (GENERATED_CODE FALSE PARENT_SCOPE)

	flex_target (TOMLScanner ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c)
	bison_target (TOMLParser
		      ${CMAKE_CURRENT_SOURCE_DIR}/parser.y
		      ${CMAKE_CURRENT_BINARY_DIR}/parser.c
		      VERBOSE
		      bison_verbose.txt)
	add_flex_bison_dependency (TOMLScanner TOMLParser)

	if ((NOT FLEX_TOMLScanner_DEFINED) AND (NOT BISON_TOMLParser_DEFINED))
		set (FAILURE_MESSAGE "Flex and Bison code could not be generated" PARENT_SCOPE)
		return ()
	endif ((NOT FLEX_TOMLScanner_DEFINED) AND (NOT BISON_TOMLParser_DEFINED))
	if (NOT FLEX_TOMLScanner_DEFINED)
		set (FAILURE_MESSAGE "Flex code could not be generated" PARENT_SCOPE)
		return ()
	endif (NOT FLEX_TOMLScanner_DEFINED)
	if (NOT BISON_TOMLParser_DEFINED)
		set (FAILURE_MESSAGE "Bison code could not be generated" PARENT_SCOPE)
		return ()
	endif (NOT BISON_TOMLParser_DEFINED)

	set (FLEX_OUTPUT_SOURCE ${FLEX_TOMLScanner_OUTPUTS} PARENT_SCOPE)
	set (FLEX_OUTPUT_HEADER ${FLEX_TOMLScanner_OUTPUT_HEADER} PARENT_SCOPE)
	set (BISON_OUTPUT_SOURCE ${BISON_TOMLParser_OUTPUT_SOURCE} PARENT_SCOPE)
	set (BISON_OUTPUT_HEADER ${BISON_TOMLParser_OUTPUT_HEADER} PARENT_SCOPE)

	set (GENERATED_CODE TRUE PARENT_SCOPE)

endfunction (generate_code)

if (DEPENDENCY_PHASE)

	check_dependencies ()
	if (NOT FOUND_DEPENDENCIES)
		remove_plugin (toml ${FAILURE_MESSAGE})
	else (NOT FOUND_DEPENDENCIES)
		generate_code ()
		if (NOT GENERATED_CODE)
			remove_plugin (toml ${FAILURE_MESSAGE})
		endif (NOT GENERATED_CODE)
	endif (NOT FOUND_DEPENDENCIES)

	set (SOURCE_FILES
	     toml.h
	     toml.c
	     driver.h
	     driver.c
	     scalar.h
	     scalar.c
	     table_array.h
	     table_array.c
	     comment_list.h
	     comment_list.c
	     codepoint.h
	     codepoint.c
	     utility.h
	     utility.c
		 write.h
		 write.c
		 type.h
		 type.c
		 null_indicator.h
	     error.h
	     error.c
	     ${FLEX_OUTPUT_HEADER}
	     ${FLEX_OUTPUT_SOURCE}
	     ${BISON_OUTPUT_HEADER}
	     ${BISON_OUTPUT_SOURCE})

endif (DEPENDENCY_PHASE)

add_plugin (toml
		ADD_TEST
		INSTALL_TEST_DATA
		LINK_ELEKTRA elektra-meta
	    SOURCES ${SOURCE_FILES}
	    INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR})
