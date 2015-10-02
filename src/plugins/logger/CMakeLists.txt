include (LibAddMacros)

add_plugin(logger
	SOURCES
		logger.h
		logger.c
	)

add_plugintest(logger)
