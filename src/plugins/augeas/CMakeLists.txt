find_package(Augeas REQUIRED)
find_package (LibXml2 REQUIRED)

if(LIBAUGEAS_FOUND AND LIBXML2_FOUND)
  include (LibAddMacros)
  
  add_includes (elektra-full ${LIBAUGEAS_INCLUDE_DIR})
  add_includes (elektra-full ${LIBXML2_INCLUDE_DIR})
  
  include_directories (${LIBAUGEAS_INCLUDE_DIR})
  include_directories (${LIBXML2_INCLUDE_DIR})
  

  set (SOURCES augeas.h augeas.c)
  add_sources (elektra-full ${SOURCES})
  add_headers (SOURCES)

  set (PLUGIN_NAME elektra-augeas)
  add_library (${PLUGIN_NAME} MODULE ${SOURCES})
  target_link_libraries (${PLUGIN_NAME} elektra)
  target_link_libraries (${PLUGIN_NAME} ${LIBAUGEAS_LIBRARIES})

  install (TARGETS ${PLUGIN_NAME}
    DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

  add_libraries (elektra-full ${LIBAUGEAS_LIBRARIES})
  add_libraries(elektra-full ${LIBXML2_LIBRARIES})


else (LIBAUGEAS_FOUND AND LIBXML2_FOUND)
  remove_plugin (augeas "augeas not found")
endif (LIBAUGEAS_FOUND AND LIBXML2_FOUND)
