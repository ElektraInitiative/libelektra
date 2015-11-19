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
	message (WARNING "did not find ronn, cannot build man pages even though requested with BUILD_DOCUMENTATION.")
endif ()
