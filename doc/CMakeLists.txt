find_package(Doxygen)

if (DOXYGEN_FOUND)
	set (OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
	set (SOURCE_DIRECTORY ${CMAKE_SOURCE_DIR}/src)

	find_package(LATEX)

	if (LATEX_FOUND)
		set (WITH_LATEX YES)
	else (LATEX_FOUND)
		set (WITH_LATEX NO)
	endif (LATEX_FOUND)

	if (DOXYGEN_DOT_FOUND)
		set (WITH_DOT YES)
	else (DOXYGEN_DOT_FOUND)
		set (WITH_DOT NO)
	endif (DOXYGEN_DOT_FOUND)

	find_package(Perl)

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile"
		"${CMAKE_CURRENT_BINARY_DIR}/Doxyfile"
		@ONLY
	)
endif (DOXYGEN_FOUND)

