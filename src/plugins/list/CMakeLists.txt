include (LibAddMacros)

add_plugin(list
	SOURCES
		list.h
		list.c
	LINK_ELEKTRA
		elektra-kdb
)

list(FIND PLUGINS "rename" RENAME_INDEX)
list(FIND PLUGINS "keytometa" KEYTOMETA_INDEX)

if(BUILD_TESTING AND RENAME_INDEX AND KEYTOMETA_INDEX GREATER -1)
	add_plugintest(list)
else()
	message (WARNING "rename and keytometa plugins are required for testing, test deactivated")
endif()
