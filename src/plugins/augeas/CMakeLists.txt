find_package(Augeas REQUIRED)
find_package (LibXml2 REQUIRED)

if(LIBAUGEAS_FOUND AND LIBXML2_FOUND)

	try_compile (HAS_LIBAUGEAS16
		"${CMAKE_BINARY_DIR}"
		"${PROJECT_SOURCE_DIR}/src/plugins/augeas/testaugeas.c"
		CMAKE_FLAGS 
			-DINCLUDE_DIRECTORIES:STRING=${LIBAUGEAS_INCLUDE_DIR}\;${LIBXML2_INCLUDE_DIR} 
			-DLINK_LIBRARIES:PATH=${LIBAUGEAS_LIBRARIES}\;${LIBXML2_LIBRARIES}
		)

	if (HAS_LIBAUGEAS16)

		include (LibAddMacros)
		
		add_plugin(augeas
			SOURCES
				augeas.h
				augeas.c
			INCLUDE_DIRECTORIES
				${LIBAUGEAS_INCLUDE_DIR}
				${LIBXML2_INCLUDE_DIR}
			LINK_LIBRARIES
				${LIBAUGEAS_LIBRARIES}
				${LIBXML2_LIBRARIES}
			)


		add_plugintest(augeas)
	else (HAS_LIBAUGEAS16)
		remove_plugin (augeas "symbols aug_text_store and aug_text_retrieve not found. Make sure you have libaugeas 0.16 or higher. This library version is included in Augeas 1.0 or higher")
	endif (HAS_LIBAUGEAS16)
else (LIBAUGEAS_FOUND AND LIBXML2_FOUND AND)
	remove_plugin (augeas "augeas not found")
endif (LIBAUGEAS_FOUND AND LIBXML2_FOUND )
