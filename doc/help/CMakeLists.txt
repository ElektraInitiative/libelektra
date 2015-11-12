find_program(RONN_LOC ronn)

if (RONN_LOC)

	file (GLOB NAMES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.md)
	foreach (FULLNAME ${NAMES})
		get_filename_component (NAME ${FULLNAME} NAME_WE)
		generate_manpage (${NAME})
	endforeach ()
else()
	message(WARNING "did not find ronn, will not build man pages")
endif()
