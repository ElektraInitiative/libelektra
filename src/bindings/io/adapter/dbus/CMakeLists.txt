include (LibAddMacros)

find_package (DBus)

if (DBUS_FOUND)


	set (ADAPTER_NAME dbus)

	## Build library

	set (ADAPTER_HDR_FILES
		dbus.h
	)
	set (ADAPTER_SRC_FILES
		dbus.c
	)

	add_headers (ELEKTRA_HEADERS)
	set (SOURCES ${ADAPTER_SRC_FILES} ${ADAPTER_HDR_FILES} ${ELEKTRA_HEADERS})

	set (ADAPTER_LIBRARY elektra-io-adapter-${ADAPTER_NAME})

	add_library (${ADAPTER_LIBRARY} SHARED ${SOURCES})
	target_include_directories (${ADAPTER_LIBRARY} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
	target_include_directories (${ADAPTER_LIBRARY} SYSTEM PUBLIC ${DBUS_INCLUDE_DIR} ${DBUS_ARCH_INCLUDE_DIR})

	#target_compile_options (${ADAPTER_LIBRARY} PUBLIC ${LIBUV_CFLAGS_OTHER})
	target_link_elektra (${ADAPTER_LIBRARY} elektra-io ${DBUS_LIBRARIES})

	include_directories(SYSTEM ${CMAKE_CURRENT_SOURCE_DIR})

	install (
		TARGETS ${ADAPTER_LIBRARY}
		LIBRARY DESTINATION lib${LIB_SUFFIX}
	)
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

	## Build test
	if (0)
		set (testexename testio_adapter_${ADAPTER_NAME})

		set (TEST_SOURCES $<TARGET_OBJECTS:cframework>)
		add_headers (TEST_SOURCES)
		file (GLOB IO_TEST_SRC_FILES "${CMAKE_SOURCE_DIR}/src/bindings/io/test/test*.c")
		list (APPEND TEST_SOURCES ${IO_TEST_SRC_FILES})
		list (APPEND TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/testio_${ADAPTER_NAME}.c")

		add_executable (${testexename} ${TEST_SOURCES})
		add_dependencies (${testexename} kdberrors_generated)

		target_include_directories (${testexename} PUBLIC "${CMAKE_SOURCE_DIR}/tests/cframework")

		target_link_elektra (${testexename} elektra-kdb elektra-plugin elektra-io ${ADAPTER_LIBRARY} m)

		add_test (NAME ${testexename}
			COMMAND "${CMAKE_BINARY_DIR}/bin/${testexename}" "${CMAKE_CURRENT_SOURCE_DIR}"
			WORKING_DIRECTORY "${WORKING_DIRECTORY}"
		)
	endif ()

	#add_subdirectory (example)

endif ()
