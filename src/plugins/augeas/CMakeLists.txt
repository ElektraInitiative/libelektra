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

		add_includes (elektra-full ${LIBAUGEAS_INCLUDE_DIR})
		add_includes (elektra-full ${LIBXML2_INCLUDE_DIR})
		
		
		include_directories (${LIBAUGEAS_INCLUDE_DIR})
		include_directories (${LIBXML2_INCLUDE_DIR})
		
		add_libraries (elektra-full ${LIBAUGEAS_LIBRARIES})
		add_libraries(elektra-full ${LIBXML2_LIBRARIES})
		
		generate_readme (augeas)
				
		set (SOURCES augeas.h augeas.c)
		add_sources (elektra-full ${SOURCES})
		add_headers (SOURCES)
		
		set (PLUGIN_NAME elektra-augeas)
		add_library (${PLUGIN_NAME} MODULE ${SOURCES})
		target_link_libraries (${PLUGIN_NAME} elektra)
		target_link_libraries (${PLUGIN_NAME} ${LIBAUGEAS_LIBRARIES})

		add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
		include_directories (${CMAKE_CURRENT_BINARY_DIR})

		install(DIRECTORY augeas DESTINATION ${TARGET_TEST_DATA_FOLDER})
		install (TARGETS ${PLUGIN_NAME}
			DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

		add_plugintest(augeas)
	else (HAS_LIBAUGEAS16)
		remove_plugin (augeas "symbols aug_text_store and aug_text_retrieve not found. Make sure you have libaugeas 0.16 or higher. This library version is included in Augeas 1.0 or higher")
	endif (HAS_LIBAUGEAS16)
else (LIBAUGEAS_FOUND AND LIBXML2_FOUND AND)
	remove_plugin (augeas "augeas not found")
endif (LIBAUGEAS_FOUND AND LIBXML2_FOUND )
