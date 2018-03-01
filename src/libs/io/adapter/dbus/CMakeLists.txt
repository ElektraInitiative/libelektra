include (LibAddMacros)

find_package (DBus)

if (DBUS_FOUND)

	set (ADAPTER_NAME dbus)

	## Build library
	set (ADAPTER_HDR_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/dbus.h"
	)
	set (ADAPTER_SRC_FILES
		"${CMAKE_CURRENT_SOURCE_DIR}/dbus.c"
	)

	set (SOURCES ${ADAPTER_SRC_FILES} ${ADAPTER_HDR_FILES})

	set (ADAPTER_LIBRARY elektra-io-adapter-${ADAPTER_NAME})

	add_lib (io-adapter-${ADAPTER_NAME}
		SOURCES ${SOURCES}
		LINK_ELEKTRA
			elektra-io
		LINK_LIBRARIES
			${DBUS_LIBRARIES}
	)
	target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})

	if (BUILD_FULL OR BUILD_STATIC)
		add_includes ("elektra-full" "${DBUS_INCLUDE_DIR};${DBUS_ARCH_INCLUDE_DIR}")
	endif ()
	#target_include_directories (${ADAPTER_LIBRARY} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

	#target_compile_options (${ADAPTER_LIBRARY} PUBLIC ${LIBUV_CFLAGS_OTHER})
	#target_link_elektra (${ADAPTER_LIBRARY} elektra-io ${DBUS_LIBRARIES})

	#install (
	#	TARGETS ${ADAPTER_LIBRARY}
	#	LIBRARY DESTINATION lib${LIB_SUFFIX}
  #)
  #
	# install (
	# 	FILES ${ADAPTER_HDR_FILES}
	# 	DESTINATION include/${TARGET_INCLUDE_FOLDER}
	# )
  #
	# configure_file (
	# 	"${CMAKE_CURRENT_SOURCE_DIR}/${ADAPTER_LIBRARY}.pc.in"
	# 	"${CMAKE_CURRENT_BINARY_DIR}/${ADAPTER_LIBRARY}.pc"
	# 	@ONLY
	# )
  #
	# install (
	# 	FILES "${CMAKE_CURRENT_BINARY_DIR}/${ADAPTER_LIBRARY}.pc"
	# 	DESTINATION lib${LIB_SUFFIX}/${TARGET_PKGCONFIG_FOLDER}
	# )

endif ()
