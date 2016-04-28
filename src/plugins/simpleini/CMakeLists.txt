include (LibAddMacros)

# silently ignore plugin on non-glibc systems
if (HAVE_GLIBC)
	add_plugin (simpleini
		SOURCES
			simpleini.h
			simpleini.c
		)
endif()

#add_plugintest (simpleini)
