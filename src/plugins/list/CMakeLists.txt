include (LibAddMacros)

add_plugin(list
	SOURCES
		list.h
		list.c
	LINK_ELEKTRA
		elektra-kdb
)

list(FIND PLUGINS "rename" _rename)
list(FIND PLUGINS "keytometa" _keytometa)

if(_rename AND _keytometa GREATER -1)
	add_plugintest(list)
else()
	message (STATUS "Disabling Plugin-Test of list, depends on: rename, keytometa")
endif()
