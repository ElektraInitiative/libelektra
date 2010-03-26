project (examples)

#find_package (Elektra)

if (ELEKTRA_FOUND)
	include (${Elektra_USE_FILE})
	add_executable (hello hello.c)
	target_link_libraries (hello elektra)
endif (ELEKTRA_FOUND)
