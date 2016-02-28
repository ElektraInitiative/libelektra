include (LibAddMacros)

# silently ignore plugin on apple
if(NOT APPLE)
	add_plugin (simpleini
		SOURCES
			simpleini.h
			simpleini.c
		)
endif()

#add_plugintest (simpleini)
