include(LibAddPlugin)

set (SOURCES resolver.h resolver.c filename.c lock.c)


#configure_file(resolverconfig.h)# TODO: for every variant
add_plugin(resolver
	SOURCES
		${SOURCES}
	LINK_LIBRARIES
		elektra
	INCLUDE_DIRECTORIES
		${CMAKE_BINARY_DIR}/src/include
		${CMAKE_SOURCE_DIR}/src/include
	COMPILE_DEFINITIONS
		USERVARIANT=\"pb\"
		SYSTEMVARIANT=\"pb\")

add_plugintest(resolver)

foreach (plugin ${PLUGINS})
	if(${plugin} MATCHES "resolver_.*")
		string(REGEX REPLACE "resolver_" "" variant ${plugin})
		string(REGEX REPLACE "_" "-" variantname ${variant})
		string(REGEX REPLACE "_.*" "" uservariant ${variant})
		string(REGEX REPLACE ".*_" "" systemvariant ${variant})

		add_plugin(${variantname}
			SOURCES
				${SOURCES}
			LINK_LIBRARIES
				elektra
			INCLUDE_DIRECTORIES
				${CMAKE_BINARY_DIR}/src/include
				${CMAKE_SOURCE_DIR}/src/include
			COMPILE_DEFINITIONS
				USERVARIANT=\"${uservariant}\"
				SYSTEMVARIANT=\"${systemvariant}\")
	endif()
endforeach(plugin)

