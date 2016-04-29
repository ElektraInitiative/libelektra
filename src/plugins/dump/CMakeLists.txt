include (LibAddPlugin)

add_plugin (dump
	SOURCES
		dump.hpp
		dump.cpp
	)

add_plugintest (dump)
