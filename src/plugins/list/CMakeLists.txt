include (LibAddMacros)

list(FIND PLUGINS "rename" _rename)
list(FIND PLUGINS "keytometa" _keytometa)

if(_rename GREATER -1 AND _keytometa GREATER -1)
	add_plugin(list
		SOURCES
			list.h
			list.c
		LINK_ELEKTRA
			elektra-kdb
	)
	
	add_plugintest(list)
else()
	remove_plugin(list "needs plugins: rename, keytometa")
endif()