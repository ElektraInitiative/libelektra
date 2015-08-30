if (BUILD_SHARED AND ENABLE_CXX11)
	add_subdirectory(include)
	include_directories(include)
	add_subdirectory(src)
	add_subdirectory(examples)

	if (ENABLE_TESTING)
		if (BUILD_FULL OR BUILD_STATIC)
			add_subdirectory (tests)
		endif (BUILD_FULL OR BUILD_STATIC)
	endif(ENABLE_TESTING)

	find_program(RONN_LOC ronn)
	if (RONN_LOC)
		set(SECTION 1)
		set(MDFILE ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
		set(OUTFILE ${CMAKE_CURRENT_BINARY_DIR}/elektrify-getenv.${SECTION})

		add_custom_command(
			OUTPUT ${OUTFILE}
			DEPENDS ${MDFILE}
			COMMAND ${RONN_LOC}
			ARGS -r --pipe ${MDFILE} > ${OUTFILE}
			)
		add_custom_target(man-elektrify-getenv ALL DEPENDS ${OUTFILE})
		add_dependencies(man man-elektrify-getenv)

		install(
			FILES ${OUTFILE}
			DESTINATION share/man/man${SECTION}
			)
	else(RONN_LOC)
		message(WARNING "did not find ronn")
	endif(RONN_LOC)
endif ()
