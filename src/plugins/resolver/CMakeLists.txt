include(LibAddPlugin)

set (SOURCES resolver.h resolver.c filename.c)

include_directories(${CMAKE_SOURCE_DIR}/src/include)
include_directories(${CMAKE_BINARY_DIR}/src/include)

add_library(elektra-resolver-lock STATIC lock.c)
set_property(TARGET elektra-resolver-lock APPEND PROPERTY COMPILE_FLAGS -fPIC)

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
		string(REGEX REPLACE "_" ";" variantlist ${variant})

		string(FIND "${basevariant}" "c" out_var_n)
		if (out_var_n)
			set (FURTHER_DEFINITIONS "ELEKTRA_CONFLICT_DEBUG")
		endif ()

		list(GET variantlist 0 variant_base)
		list(GET variantlist 1 variant_user)
		list(GET variantlist 2 variant_system)

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
				ELEKTRA_VARIANT_BASE=\"${variant_base}\"
				ELEKTRA_VARIANT_USER=\"${variant_user}\"
				ELEKTRA_VARIANT_SYSTEM=\"${variant_system}\"
				ELEKTRA_VARIANT=${variant}
				ELEKTRA_PLUGIN_NAME=\"${plugin}\"
				${FURTHER_DEFINITIONS})
	endif()
endforeach(plugin)

