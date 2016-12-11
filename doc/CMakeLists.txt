find_package(Doxygen)

# do_doc
#
#builds and install documentation
#
#target  is man, html or latex
#folder  is the folder where it will be built
#file    is the file name to set a dependency to
#install is where it should be installed
#
macro (do_doc target folder file install)
	# sometimes doxygen is too slow and fails with "Could not create output directory .../doc/html"
	file(MAKE_DIRECTORY ${folder})

	#get_filename_component (name ${target} NAME_WE)

	add_custom_target (${target} ALL
		DEPENDS ${folder}/${file}
	)

	if (INSTALL_DOCUMENTATION)
		install (
			DIRECTORY ${folder}
			DESTINATION ${install}
		)
	endif (INSTALL_DOCUMENTATION)
endmacro (do_doc)

if (DOXYGEN_FOUND)
	set (OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	if (BUILD_PDF)
		find_package(LATEX)
		if (LATEX_COMPILER)
			set (WITH_LATEX YES)
		else (LATEX_COMPILER)
			set (WITH_LATEX NO)
			message (WARNING "Sorry, I cannot create the PDF Manual: I did not find Latex.")
		endif (LATEX_COMPILER)
	else (BUILD_PDF)
		set (WITH_LATEX NO)
	endif (BUILD_PDF)

	if (DOXYGEN_DOT_FOUND)
		set (WITH_DOT YES)
	else (DOXYGEN_DOT_FOUND)
		set (WITH_DOT NO)
	endif (DOXYGEN_DOT_FOUND)

	find_package(Perl)

	add_executable (markdownlinkconverter markdownlinkconverter/markdownlinkconverter.c)
	# fix usage with wine
	# https://github.com/ElektraInitiative/libelektra/pull/340#discussion_r44044444
	find_util (markdownlinkconverter MARKDOWN_LINK_CONVERTER "")

	configure_file (
		"${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile"
		"${CMAKE_CURRENT_BINARY_DIR}/Doxyfile"
		@ONLY
	)

	do_doc(html ${OUTPUT_DIRECTORY}/html/ index.html
		${TARGET_DOCUMENTATION_HTML_FOLDER})
	do_doc(man3 ${OUTPUT_DIRECTORY}/man/man3elektra/ kdb.3elektra
		"${TARGET_DOCUMENTATION_MAN_FOLDER};PATTERN;_*;EXCLUDE")
	add_dependencies(man3 html)
	set(outputs ${OUTPUT_DIRECTORY}/html/index.html
		${OUTPUT_DIRECTORY}/man/man3elektra/kdb.3elektra)

	if (WITH_LATEX)
		do_doc(latex ${OUTPUT_DIRECTORY}/latex/ refman.tex
			${TARGET_DOCUMENTATION_LATEX_FOLDER})
		add_dependencies(latex man3 html)
		list(APPEND outputs ${OUTPUT_DIRECTORY}/latex/refman.tex)

		file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/markdownlinkconverter/elektraSpecialCharacters.sty"
				DESTINATION "${OUTPUT_DIRECTORY}/latex/")

		add_custom_command (
			OUTPUT ${OUTPUT_DIRECTORY}/latex/refman.pdf
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.tex markdownlinkconverter
			WORKING_DIRECTORY ${OUTPUT_DIRECTORY}/latex/
		)

		add_custom_target (pdf ALL
			DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.pdf
		)
		add_dependencies(pdf latex)

	endif (WITH_LATEX)

	add_custom_command (
		OUTPUT ${outputs}
		COMMAND ${DOXYGEN_EXECUTABLE}
		ARGS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile markdownlinkconverter
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	)
else (DOXYGEN_FOUND)
	message (WARNING "Sorry, I cannot create the reference manual: I could not find Doxygen")
endif (DOXYGEN_FOUND)

if (INSTALL_DOCUMENTATION)
	#API.md
	install (FILES BIGPICTURE.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	#code_of_conduct.md
	#COMPILE.md
	install (FILES GOALS.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	install (FILES LICENSE.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	install (FILES METADATA.ini DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	install (FILES SECURITY.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	#TESTING.md
	#VERSION.md
	install (FILES AUTHORS DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	#CODING.md
	install (FILES CONTRACT.ini DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	#DESIGN.md
	#GIT.md
	#INSTALL.md
	install (FILES NEWS.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})
	install (FILES WHY.md DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER})

endif ()

add_subdirectory(help)
