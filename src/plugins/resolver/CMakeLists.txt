include(LibAddPlugin)

set (SOURCES resolver.h resolver.c filename.c)

find_package(Threads)

#TODO: remove "default" resolver
add_plugin(resolver
	SOURCES
		${SOURCES}
	LINK_LIBRARIES
		${CMAKE_THREAD_LIBS_INIT}
		${CMAKE_REALTIME_LIBS_INIT}
	COMPILE_DEFINITIONS
		ELEKTRA_VARIANT_BASE=\"\"
		ELEKTRA_VARIANT_USER=\"hpu\"
		ELEKTRA_VARIANT_SYSTEM=\"b\"
		ELEKTRA_PLUGIN_NAME=\"resolver\"
		ELEKTRA_LOCK_FILE
		ELEKTRA_LOCK_MUTEX
	)

add_plugintest(resolver)

foreach (plugin ${PLUGINS})
	if (${plugin} MATCHES "^resolver_.*")
		string(REGEX REPLACE "resolver_" "" variant ${plugin})
		string(REGEX REPLACE "_" ";" variantlist ${variant})

		list(GET variantlist 0 variant_base)
		list(GET variantlist 1 variant_user)
		list(GET variantlist 2 variant_system)

		# don't forget near-global scope for CMake variables
		set (FURTHER_DEFINITIONS "")
		set (FURTHER_LIBRARIES "")

		string(FIND "${variant_base}" "f" out_var_n)
		if (NOT "${out_var_n}" EQUAL "-1")
			set (FURTHER_DEFINITIONS ${FURTHER_DEFINITIONS} "ELEKTRA_LOCK_FILE")
		endif ()

		string(FIND "${variant_base}" "m" out_var_n)
		if (NOT "${out_var_n}" EQUAL "-1")
			set (FURTHER_DEFINITIONS ${FURTHER_DEFINITIONS} "ELEKTRA_LOCK_MUTEX")
			set(FURTHER_LIBRARIES ${FURTHER_LIBRARIES}
				${CMAKE_THREAD_LIBS_INIT}
				${CMAKE_REALTIME_LIBS_INIT})
		endif ()


		add_plugin(${plugin}
			SOURCES
				${SOURCES}
			LINK_LIBRARIES
				${FURTHER_LIBRARIES}
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

