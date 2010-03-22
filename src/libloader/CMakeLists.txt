include (LibAddMacros)

add_sources (elektra-full "static.c")

find_package (DL REQUIRED)
if (DL_FOUND)
	add_sources (elektra-shared "dl.c")
	add_includes (elektra ${DL_INCLUDE_DIRS})
	add_libraries (elektra ${DL_LIBRARIES})
else (DL_FOUND)
	message (ERROR "dl not found and no other module mechanism currently defined, see src/libloader")
endif (DL_FOUND)
