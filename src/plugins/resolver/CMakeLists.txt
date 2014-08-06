include(LibAddPlugin)

set (SOURCES resolver.h resolver.c filename.c)

include_directories(${CMAKE_SOURCE_DIR}/src/include)
include_directories(${CMAKE_BINARY_DIR}/src/include)

add_library(elektra-resolver-lock STATIC lock.c)
set_property(TARGET elektra-resolver-lock APPEND PROPERTY COMPILE_FLAGS -fPIC)

#configure_file(resolverconfig.h)# TODO: for every variant
add_plugin(resolver
	SOURCES
		${SOURCES}
	LINK_LIBRARIES
		elektra
		elektra-resolver-lock
	INCLUDE_DIRECTORIES
		${CMAKE_BINARY_DIR}/src/include
		${CMAKE_SOURCE_DIR}/src/include
	COMPILE_DEFINITIONS
		ELEKTRA_VARIANT_BASE=\"\"
		ELEKTRA_VARIANT_USER=\"hub\"
		ELEKTRA_VARIANT_SYSTEM=\"b\"
		ELEKTRA_PLUGIN_NAME=\"resolver\")

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
				elektra-resolver-lock
			INCLUDE_DIRECTORIES
				${CMAKE_BINARY_DIR}/src/include
				${CMAKE_SOURCE_DIR}/src/include
			COMPILE_DEFINITIONS
				ELEKTRA_VARIANT_BASE=\"\"
				ELEKTRA_VARIANT_USER=\"${uservariant}\"
				ELEKTRA_VARIANT_SYSTEM=\"${systemvariant}\"
				ELEKTRA_VARIANT=${variant}
				ELEKTRA_PLUGIN_NAME=\"${plugin}\")
	endif()
endforeach(plugin)

