if (BUILD_DOCUMENTATION)
	find_program (RONN_LOC ronn)
	find_program (DIFF_COMMAND diff)

	if (RONN_LOC)
		add_custom_target (man ALL)
	else (RONN_LOC)
		message ("Sorry, I cannot find ronn (https://github.com/rtomayko/ronn), thus generating the man pages will not work. "
			 "I will install the man pages directly from the repository instead.")
	endif (RONN_LOC)
endif (BUILD_DOCUMENTATION)

file (
	GLOB KDBNAMES
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	kdb*.md)
foreach (FULLNAME ${KDBNAMES})
	get_filename_component (NAME ${FULLNAME} NAME_WE)
	generate_manpage (${NAME} COMPONENT elektra-bin)
endforeach ()

file (
	GLOB KDBNAMES
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	elektra*.md)
foreach (FULLNAME ${KDBNAMES})
	get_filename_component (NAME ${FULLNAME} NAME_WE)
	generate_manpage (${NAME} SECTION 7 COMPONENT elektra-doc)
endforeach ()
