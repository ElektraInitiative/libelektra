include (LibAddMacros)
include (LibAddBinding)

## Build example_notification
set (example example_notification)

# TODO verify that BUILD_STATIC / BUILD_FULL works
SET (SRC_FILES
  example_notification.c
)
add_headers (ELEKTRA_HEADERS)
set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})

add_executable (${example} ${SOURCES})
add_dependencies (${example} kdberrors_generated)

# target_include_directories (${example} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

target_link_elektra (${example} elektra-kdb elektra-notification)

# The asynchronous example requires io_uv
check_binding_included ("io_uv" IS_INCLUDED SUBDIRECTORY "io/uv")
find_package (PkgConfig)
pkg_check_modules (LIBUV QUIET libuv>=1)
if (IS_INCLUDED AND LIBUV_FOUND)
  ## Build example_notification_async
  set (example example_notification_async)

  SET (SRC_FILES
    example_notification_async.c
  )
  add_headers (ELEKTRA_HEADERS)
  set (SOURCES ${SRC_FILES} ${ELEKTRA_HEADERS})

  add_executable (${example} ${SOURCES})
  add_dependencies (${example} kdberrors_generated)

  # target_include_directories (${example} SYSTEM PUBLIC ${LIBUV_INCLUDE_DIRS})

  target_link_elektra (${example} elektra-kdb elektra-notification elektra-io elektra-io-uv)
endif ()
