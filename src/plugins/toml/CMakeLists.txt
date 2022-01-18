function (check_dependencies)
	set (
		FOUND_DEPENDENCIES
		FALSE
		PARENT_SCOPE)

	set (USE_HOMEBREW_FLEX ${APPLE} AND EXISTS /usr/local/opt/flex)
	if (USE_HOMEBREW_FLEX)
		list (APPEND CMAKE_PREFIX_PATH "/usr/local/opt/flex")
	endif (USE_HOMEBREW_FLEX)
	find_package (FLEX 2.6.2 QUIET)

	set (USE_HOMEBREW_BISON ${APPLE} AND EXISTS /usr/local/opt/bison)
	if (USE_HOMEBREW_BISON)
		list (APPEND CMAKE_PREFIX_PATH "/usr/local/opt/bison")
	endif (USE_HOMEBREW_BISON)
	find_package (BISON 3 QUIET)

	if ((NOT FLEX_FOUND) AND (NOT BISON_FOUND))
		set (
			FAILURE_MESSAGE
			"Flex and Bison could not be found"
			PARENT_SCOPE)
		return ()
	endif ((NOT FLEX_FOUND) AND (NOT BISON_FOUND))
	if (NOT FLEX_FOUND)
		set (
			FAILURE_MESSAGE
			"Flex could not be found"
			PARENT_SCOPE)
		return ()
	endif (NOT FLEX_FOUND)
	if (NOT BISON_FOUND)
		set (
			FAILURE_MESSAGE
			"Bison could not be found"
			PARENT_SCOPE)
		return ()
	endif (NOT BISON_FOUND)

	set (
		FOUND_DEPENDENCIES
		TRUE
		PARENT_SCOPE)
endfunction (check_dependencies)

function (generate_code)
	set (
		GENERATED_CODE
		FALSE
		PARENT_SCOPE)

	flex_target (TOMLScanner ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c)
	bison_target (TOMLParser ${CMAKE_CURRENT_SOURCE_DIR}/parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.c VERBOSE bison_verbose.txt)
	add_flex_bison_dependency (TOMLScanner TOMLParser)

	if ((NOT FLEX_TOMLScanner_DEFINED) AND (NOT BISON_TOMLParser_DEFINED))
		set (
			FAILURE_MESSAGE
			"Flex and Bison code could not be generated"
			PARENT_SCOPE)
		return ()
	endif ((NOT FLEX_TOMLScanner_DEFINED) AND (NOT BISON_TOMLParser_DEFINED))
	if (NOT FLEX_TOMLScanner_DEFINED)
		set (
			FAILURE_MESSAGE
			"Flex code could not be generated"
			PARENT_SCOPE)
		return ()
	endif (NOT FLEX_TOMLScanner_DEFINED)
	if (NOT BISON_TOMLParser_DEFINED)
		set (
			FAILURE_MESSAGE
			"Bison code could not be generated"
			PARENT_SCOPE)
		return ()
	endif (NOT BISON_TOMLParser_DEFINED)

	set (
		FLEX_OUTPUT_SOURCE
		${FLEX_TOMLScanner_OUTPUTS}
		PARENT_SCOPE)
	set (
		FLEX_OUTPUT_HEADER
		${FLEX_TOMLScanner_OUTPUT_HEADER}
		PARENT_SCOPE)
	set (
		BISON_OUTPUT_SOURCE
		${BISON_TOMLParser_OUTPUT_SOURCE}
		PARENT_SCOPE)
	set (
		BISON_OUTPUT_HEADER
		${BISON_TOMLParser_OUTPUT_HEADER}
		PARENT_SCOPE)

	set (
		GENERATED_CODE
		TRUE
		PARENT_SCOPE)

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

	set (
		SOURCE_FILES
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
		utf8_dfa.h
		codepoint.h
		codepoint.c
		utility.h
		utility.c
		write.h
		write.c
		node.h
		node.c
		prepare.c
		prepare.h
		integer.h
		integer.c
		type.h
		type.c
		error.h
		${FLEX_OUTPUT_HEADER}
		${FLEX_OUTPUT_SOURCE}
		${BISON_OUTPUT_HEADER}
		${BISON_OUTPUT_SOURCE})

endif (DEPENDENCY_PHASE)

add_plugin (
	toml ADD_TEST INSTALL_TEST_DATA TEST_README
	TEST_REQUIRED_PLUGINS type base64
	LINK_ELEKTRA elektra-meta
	SOURCES ${SOURCE_FILES}
	INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR} COMPONENT libelektra${SO_VERSION})
