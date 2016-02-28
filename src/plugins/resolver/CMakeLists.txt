include (LibAddPlugin)

if (DEPENDENCY_PHASE)
	find_package (Threads)
	add_plugintest (resolver)
endif ()

# TODO: remove resolver
# following test cases still fail (testmod_resolver testtool_backend testkdb_simple)
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
	CATEGORIES
		"RESOLVER"
		"NODEP"
		"MINIMAL"
	)


set (SOURCES resolver.h resolver.c filename.c)

if (KDB_DEFAULT_RESOLVER MATCHES "resolver_.*")
	set (RESOLVERS "${KDB_DEFAULT_RESOLVER}") # default resolver
endif ()

foreach (plugin ${PLUGINS})
	if (${plugin} MATCHES "^resolver_.*")
		set (RESOLVERS
			"${RESOLVERS}"
			"${plugin}"
			)
	endif ()
	if (${plugin} MATCHES "ALL" OR ${plugin} MATCHES "DEFAULT")
		set (RESOLVERS
			"${RESOLVERS}"
			resolver_fm_b_b
			resolver_fm_pb_b
			resolver_fm_hb_b
			resolver_fm_hp_b
			resolver_fm_ub_x
			resolver_fm_xb_x
			resolver_fm_xp_x
			resolver_fm_xhp_x
			resolver_fm_uhb_xb
			resolver_fm_hpu_b # proposed default
			)
	endif ()
endforeach ()

list (REMOVE_DUPLICATES RESOLVERS)

foreach (plugin ${RESOLVERS})
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
			CATEGORIES
				"RESOLVER"
				"NODEP"
			)
	endif()
endforeach(plugin)

