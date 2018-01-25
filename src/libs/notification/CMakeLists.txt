set (SOURCES
  notification.c
)

set (LIBRARY_NAME elektra-notification)

add_lib (notification
  SOURCES ${SOURCES}
  #LINK_ELEKTRA elektra elektra-kdb
  LINK_LIBRARIES elektra-kdb elektra-ease
)
# target_link_libraries (elektra-notification elektra-kdb)
# get_target_property(ALL_DEPENDENCIES1 Library1 LINK_LIBRARIES)

configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc.in"
  "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc"
  @ONLY
)

install (
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${LIBRARY_NAME}.pc"
  DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
)

add_subdirectory (example)
