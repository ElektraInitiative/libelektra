if (BUILD_DOCUMENTATION)
	find_program (RONN_LOC ronn)
	find_package (Git)

	if (RONN_LOC)
		add_custom_target (man ALL)
	else (RONN_LOC)
		message (
			"Sorry, I cannot find ronn-ng (https://github.com/apjanke/ronn-ng), "
			"thus re-generating the man pages will not work. "
			"Modifying the Markdown sources in doc/help will thus not work. "
			"Instead the existing files from doc/man will be used directly.")
	endif (RONN_LOC)

	if (NOT GIT_EXECUTABLE)
		message ("Sorry, I cannot find git, thus re-generating the man pages will not work. "
			 "Modifying the Markdown sources in doc/help will thus not work. "
			 "Instead the existing files from doc/man will be used directly.")
	endif (NOT GIT_EXECUTABLE)
endif (BUILD_DOCUMENTATION)

file (
	GLOB KDBNAMES
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	kdb*.md)
foreach (FULLNAME ${KDBNAMES})
	get_filename_component (NAME ${FULLNAME} NAME_WE)
	generate_man_page (${NAME} COMPONENT elektra-bin)
endforeach ()

file (
	GLOB KDBNAMES
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	elektra*.md)
foreach (FULLNAME ${KDBNAMES})
	get_filename_component (NAME ${FULLNAME} NAME_WE)
	generate_man_page (${NAME} SECTION 7 COMPONENT elektra-doc)
endforeach ()
