include (LibAddMacros)

add_sources (elektra-full "static.c")

if (BUILD_SHARED)
	add_sources (elektra-shared "dl.c")
	add_includes (elektra-shared ${DL_INCLUDE_DIRS})
	add_libraries (elektra-shared ${DL_LIBRARIES} ${CMAKE_DL_LIBS})
endif (BUILD_SHARED)
