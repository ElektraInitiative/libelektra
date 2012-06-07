cmake_minimum_required(VERSION 2.6)

find_package(Elektra REQUIRED)

if (Elektra_FOUND)
	include_directories (${Elektra_INCLUDE_DIR})

	add_executable (application application.c)
	target_link_libraries (application ${Elektra_LIBRARIES})
else (Elektra_FOUND)
	message ("Elektra not found")
endif(Elektra_FOUND)
