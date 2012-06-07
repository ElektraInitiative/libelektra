find_package(Doxygen)

# do_doc
#
#name is man, html or latex
#builds and install documentation
macro (do_doc target file)
	add_custom_command (
		OUTPUT ${target}/${file}
		COMMAND ${DOXYGEN_EXECUTABLE}
		ARGS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
	)

	get_filename_component (name ${target} NAME_WE)

	add_custom_target (${name} ALL
		DEPENDS ${target}/${file}
	)

	if (INSTALL_DOCUMENTATION)
		install (
			DIRECTORY ${target}
			DESTINATION ${TARGET_DOCUMENTATION_FOLDER}
		)
	endif (INSTALL_DOCUMENTATION)
endmacro (do_doc)

if (DOXYGEN_FOUND)
	set (OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	find_package(LATEX)

	if (LATEX_COMPILER)
		set (WITH_LATEX YES)
	else (LATEX_COMPILER)
		set (WITH_LATEX NO)
	endif (LATEX_COMPILER)

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

	do_doc(${OUTPUT_DIRECTORY}/html index.html)
	do_doc(${OUTPUT_DIRECTORY}/man man3/kdb.3)

	if (LATEX_COMPILER)
		do_doc(${OUTPUT_DIRECTORY}/latex refman.tex)

		add_custom_command (
			OUTPUT ${OUTPUT_DIRECTORY}/latex/refman.pdf
			COMMAND ${PDFLATEX_COMPILER}
			ARGS ${OUTPUT_DIRECTORY}/latex/refman.tex
			DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.tex
			WORKING_DIRECTORY ${OUTPUT_DIRECTORY}/latex/
		)

		add_custom_target (pdf ALL
			DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.pdf
		)
	endif (LATEX_COMPILER)
else (DOXYGEN_FOUND)
	message (STATUS "Warning, Doxygen not found, Reference Manual can't be created. But it was requested to do so.")
endif (DOXYGEN_FOUND)

