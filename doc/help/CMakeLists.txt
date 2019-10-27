if (BUILD_DOCUMENTATION)
	find_program (RONN_LOC ronn)
	find_program (GIT_COMMAND git)

	if (RONN_LOC)
		add_custom_target (man ALL)
		if (GIT_COMMAND)
			add_custom_command (TARGET man POST_BUILD
					    COMMAND ${CMAKE_COMMAND}
						    ARGS
						    -D
						    MAN_PAGE_DIRECTORY=${CMAKE_SOURCE_DIR}/doc/man
						    -D
						    GIT_COMMAND=${GIT_COMMAND}
						    -P
						    ${CMAKE_SOURCE_DIR}/scripts/cmake/ElektraManpageRevert.cmake)
		endif (GIT_COMMAND)
	else (RONN_LOC)
		message ("Sorry, I cannot find ronn (https://github.com/rtomayko/ronn), thus generating the man pages will not work. "
			 "I will install the man pages directly from the repository instead.")
	endif (RONN_LOC)
endif (BUILD_DOCUMENTATION)

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
