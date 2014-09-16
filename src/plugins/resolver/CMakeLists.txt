include(LibAddPlugin)

add_plugin_helper(elektra-resolver-lock
	SOURCES
		lock.c
	)

set (SOURCES resolver.h resolver.c filename.c)

add_plugin(resolver
	SOURCES
		${SOURCES}
	LINK_LIBRARIES
		elektra-resolver-lock
	COMPILE_DEFINITIONS
		ELEKTRA_VARIANT_BASE=\"\"
		ELEKTRA_VARIANT_USER=\"hub\"
		ELEKTRA_VARIANT_SYSTEM=\"b\"
		ELEKTRA_PLUGIN_NAME=\"resolver\"
	)

add_plugintest(resolver)

foreach (plugin ${PLUGINS})
	if(${plugin} MATCHES "resolver_.*")
		string(REGEX REPLACE "resolver_" "" variant ${plugin})
		string(REGEX REPLACE "_" ";" variantlist ${variant})

		list(GET variantlist 0 variant_base)
		list(GET variantlist 1 variant_user)
		list(GET variantlist 2 variant_system)

		# if a c is found in the first part of the variant
		# the conflict debug mode is activated
		string(FIND "${variant_base}" "c" out_var_n)
		if (NOT "${out_var_n}" EQUAL "-1")
			set (FURTHER_DEFINITIONS "ELEKTRA_CONFLICT_DEBUG")
		endif ()


		add_plugin(${plugin}
			SOURCES
				${SOURCES}
			LINK_LIBRARIES
				elektra-resolver-lock
			COMPILE_DEFINITIONS
				ELEKTRA_VARIANT_BASE=\"${variant_base}\"
				ELEKTRA_VARIANT_USER=\"${variant_user}\"
				ELEKTRA_VARIANT_SYSTEM=\"${variant_system}\"
				ELEKTRA_VARIANT=${variant}
				ELEKTRA_PLUGIN_NAME=\"${plugin}\"
				${FURTHER_DEFINITIONS}
			)
	endif()
endforeach(plugin)

