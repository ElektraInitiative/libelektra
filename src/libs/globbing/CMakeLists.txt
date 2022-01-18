file (GLOB SOURCES *.c)

safe_check_symbol_exists ("fnmatch" "fnmatch.h" HAVE_FNMATCH)

if (HAVE_FNMATCH)
	add_lib (
		globbing
		SOURCES
		${SOURCES}
		LINK_ELEKTRA
		elektra-ease
		COMPONENT
		libelektra${SO_VERSION})
else ()
	message ("Excluding lib-globbing, because fnmatch was not found.")
endif ()
