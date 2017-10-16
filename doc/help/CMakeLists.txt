find_program (RONN_LOC ronn)

if (RONN_LOC)
	add_custom_target (man ALL)


	file (GLOB KDBNAMES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} kdb*.md)
	foreach (FULLNAME ${KDBNAMES})
		get_filename_component (NAME ${FULLNAME} NAME_WE)
		generate_manpage (${NAME})
	endforeach ()

	file (GLOB KDBNAMES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} elektra*.md)
	foreach (FULLNAME ${KDBNAMES})
		get_filename_component (NAME ${FULLNAME} NAME_WE)
		generate_manpage (${NAME} SECTION 7)
	endforeach ()
else ()
	message ("Sorry, I cannot find ronn (https://github.com/rtomayko/ronn), thus generating the man pages will not work. "
		 "I will install the man pages directly from the repository instead.")
	set (MAN_PAGES_DIR "${CMAKE_SOURCE_DIR}/doc/man")

	if (INSTALL_DOCUMENTATION)
		install (DIRECTORY "${MAN_PAGES_DIR}/" DESTINATION share/man/man1 FILES_MATCHING PATTERN "*.1")
		install (DIRECTORY "${MAN_PAGES_DIR}/" DESTINATION share/man/man7 FILES_MATCHING PATTERN "*.7")
	endif (INSTALL_DOCUMENTATION)
endif ()
