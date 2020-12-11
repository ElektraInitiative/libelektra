find_package (Doxygen QUIET)

# ~~~
# do_doc
#
# builds and install documentation
#
# target  is man, html or latex
# folder  is the folder where it will be built
# file    is the file name to set a dependency to
# install is where it should be installed
# target_component is the component name for component-specifc installation
# ~~~
macro (do_doc target folder file install target_component)

	# sometimes doxygen is too slow and fails with "Could not create output directory .../doc/html"
	file (MAKE_DIRECTORY ${folder})

	# get_filename_component (name ${target} NAME_WE)

	add_custom_target (${target} ALL DEPENDS ${folder}/${file})

	if (INSTALL_DOCUMENTATION)
		install (
			DIRECTORY ${folder}
			COMPONENT ${target_component}
			DESTINATION ${install})
	endif (INSTALL_DOCUMENTATION)
endmacro (do_doc)

if (DOXYGEN_FOUND)
	set (OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

	if (BUILD_PDF)
		find_package (LATEX)
		if (LATEX_COMPILER)
			set (WITH_LATEX YES)
		else (LATEX_COMPILER)
			set (WITH_LATEX NO)
			message (WARNING "Sorry, I cannot create the PDF Manual: I did not find Latex.")
		endif (LATEX_COMPILER)
	else (BUILD_PDF)
		set (WITH_LATEX NO)
	endif (BUILD_PDF)

	if (BUILD_DOCSET)
		execute_process (
			COMMAND make ARGS -v
			RESULT_VARIABLE MAKE_NOT_AVAILABLE
			OUTPUT_QUIET)
		if (MAKE_NOT_AVAILABLE)
			set (WITH_DOCSET NO)
			message (WARNING "Sorry, since Make is not available I can not build the DocSet.")
		else (MAKE_NOT_AVAILABLE)
			set (WITH_DOCSET YES)
		endif (MAKE_NOT_AVAILABLE)
	endif (BUILD_DOCSET)

	if (DOXYGEN_DOT_FOUND)
		set (WITH_DOT YES)
	else (DOXYGEN_DOT_FOUND)
		set (WITH_DOT NO)
	endif (DOXYGEN_DOT_FOUND)

	find_package (Perl QUIET)

	add_executable (markdownlinkconverter markdownlinkconverter/markdownlinkconverter.c)

	# fix usage with wine: https://github.com/ElektraInitiative/libelektra/pull/340#discussion_r44044444
	find_util (markdownlinkconverter MARKDOWN_LINK_CONVERTER "")
	if (DOXYGEN_VERSION VERSION_GREATER "1.8.11")
		set (MARKDOWN_LINK_CONVERTER "\\\"${MARKDOWN_LINK_CONVERTER}\\\"")
	endif ()

	configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile" "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" @ONLY)
	file (
		GENERATE
		OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		INPUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

	do_doc (html ${OUTPUT_DIRECTORY}/html/ index.html ${TARGET_DOCUMENTATION_HTML_FOLDER} elektra-doc)
	do_doc (man3 ${OUTPUT_DIRECTORY}/man/man3elektra/ kdb.3elektra "${TARGET_DOCUMENTATION_MAN_FOLDER};PATTERN;_*;EXCLUDE" elektra-doc)
	add_dependencies (man3 html)
	set (outputs ${OUTPUT_DIRECTORY}/html/index.html ${OUTPUT_DIRECTORY}/man/man3elektra/kdb.3elektra)

	if (WITH_LATEX)
		do_doc (latex ${OUTPUT_DIRECTORY}/latex/ refman.tex ${TARGET_DOCUMENTATION_LATEX_FOLDER})
		add_dependencies (latex man3 html)
		list (APPEND outputs ${OUTPUT_DIRECTORY}/latex/refman.tex)

		file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/markdownlinkconverter/elektraSpecialCharacters.sty"
		      DESTINATION "${OUTPUT_DIRECTORY}/latex/")

		add_custom_command (
			OUTPUT ${OUTPUT_DIRECTORY}/latex/refman.pdf
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			COMMAND ${PDFLATEX_COMPILER} ARGS ${PDFLATEX_COMPILER_OPTIONS} ${OUTPUT_DIRECTORY}/latex/refman.tex
			DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.tex markdownlinkconverter
			WORKING_DIRECTORY ${OUTPUT_DIRECTORY}/latex/)

		add_custom_target (pdf ALL DEPENDS ${OUTPUT_DIRECTORY}/latex/refman.pdf)
		add_dependencies (pdf latex)

	endif (WITH_LATEX)

	if (WITH_DOCSET)
		set (DOCSET_PATH ${OUTPUT_DIRECTORY}/html/org.libelektra.docset)
		add_custom_command (
			OUTPUT ${DOCSET_PATH}
			COMMAND make
			COMMAND
				${CMAKE_COMMAND} ARGS -D DOCSET_PATH=${DOCSET_PATH} -D ICON_PATH=${CMAKE_SOURCE_DIR}/doc/images/icon.png -P
				${CMAKE_SOURCE_DIR}/scripts/cmake/ElektraDocSet.cmake
			DEPENDS ${OUTPUT_DIRECTORY}/html/index.html markdownlinkconverter
			WORKING_DIRECTORY ${OUTPUT_DIRECTORY}/html)
		add_custom_target (docset ALL DEPENDS ${DOCSET_PATH})
	endif (WITH_DOCSET)

	add_custom_command (
		OUTPUT ${outputs}
		COMMAND ${DOXYGEN_EXECUTABLE} ARGS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile markdownlinkconverter
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
else (DOXYGEN_FOUND)
	message (WARNING "Sorry, I cannot create the reference manual: I could not find Doxygen")
endif (DOXYGEN_FOUND)

if (INSTALL_DOCUMENTATION)
	install (
		FILES BIGPICTURE.md
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES GOALS.md
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES METADATA.ini
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES SECURITY.md
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES AUTHORS.md
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES CONTRACT.ini
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	set (news ${CMAKE_CURRENT_BINARY_DIR}/NEWS.md)
	file (GLOB files "news/*.md")
	file (REMOVE ${news})
	foreach (file ${files})
		file (READ ${file} content)
		file (APPEND ${news} "\n\n\n\n\n${content}")
	endforeach ()
	install (
		FILES ${news}
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)
	install (
		FILES WHY.md
		DESTINATION ${TARGET_DOCUMENTATION_TEXT_FOLDER}
		COMPONENT libelektra4)

endif ()

add_subdirectory (help)
