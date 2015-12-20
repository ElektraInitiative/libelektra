find_package(Threads)

if (APPLE)
	remove_tool(race "race tool does not compile under MAC OS X")
else()
	add_headers (HDR_FILES)

	add_executable(race race.c)
	tool_link_elektra(race)
	target_link_libraries(race ${CMAKE_THREAD_LIBS_INIT})
	target_link_libraries(race ${CMAKE_REALTIME_LIBS_INIT})

	install(TARGETS race DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
endif()
