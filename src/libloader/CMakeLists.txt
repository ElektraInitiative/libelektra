include (LibAddMacros)

add_sources (elektra-full "static.c")

if (BUILD_SHARED)
	add_sources (elektra-shared "dl.c")
	add_includes (elektra-shared)
	add_libraries (elektra-shared ${CMAKE_DL_LIBS})
endif (BUILD_SHARED)
