find_package(Threads)
find_package(LibRt)

if (APPLE)
	# Mac OS X does not provide pthread_barrier_t and all related functions are not to be found.
	# Thus we exlucde the race test from OS X.
	remove_tool(race "race tool does not compile under MAC OS X")
else()
	if (NOT LIBRT_FOUND)
		remove_tool(race "race tool requires librt support")
		return ()
	endif ()

	add_headers (HDR_FILES)

	add_executable(race race.c)
	tool_link_elektra(race)
	target_link_libraries(race ${CMAKE_THREAD_LIBS_INIT})
	target_link_libraries(race ${LIBRT_LIBRARIES})

	install(TARGETS race DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
endif()
