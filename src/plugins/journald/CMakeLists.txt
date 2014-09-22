find_package(SystemdJournal REQUIRED)

if(LIBSYSTEMD_JOURNAL_FOUND)
  include (LibAddMacros)
  
  generate_readme (journald)
  
  
  add_includes (elektra-full ${LIBSYSTEMD_JOURNAL_INCLUDE_DIR})
  include_directories (${LIBSYSTEMD_JOURNAL_INCLUDE_DIR})

  set (SOURCES journald.h journald.c)
  add_sources (elektra-full ${SOURCES})
  add_headers (SOURCES)

  set (PLUGIN_NAME elektra-journald)
  add_library (${PLUGIN_NAME} MODULE ${SOURCES})
  target_link_libraries (${PLUGIN_NAME} elektra)
  target_link_libraries (${PLUGIN_NAME} ${LIBSYSTEMD_JOURNAL_LIBRARIES})

  add_includes (elektra-full ${CMAKE_CURRENT_BINARY_DIR})
  include_directories (${CMAKE_CURRENT_BINARY_DIR})

  install (TARGETS ${PLUGIN_NAME}
    DESTINATION lib${LIB_SUFFIX}/${TARGET_PLUGIN_FOLDER})

  add_libraries (elektra-full ${LIBSYSTEMD_JOURNAL_LIBRARIES})

else (LIBSYSTEMD_JOURNAL_FOUND)
  remove_plugin (journald "systemd-journal not found")
endif (LIBSYSTEMD_JOURNAL_FOUND)


#add_plugintest (journald)
