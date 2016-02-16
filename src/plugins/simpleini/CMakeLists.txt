include (LibAddMacros)

if(APPLE)
	remove_plugin(simpleini "broken on Mac OS X")
else()
	add_plugin(simpleini
		SOURCES
			simpleini.h
			simpleini.c
		)
endif()

#add_plugintest (simpleini)
