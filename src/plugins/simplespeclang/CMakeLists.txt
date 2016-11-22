include (LibAddMacros)

add_plugin (simplespeclang
	SOURCES
		simplespeclang.hpp
		simplespeclang.cpp
	LINK_ELEKTRA
		elektra-meta
	)
