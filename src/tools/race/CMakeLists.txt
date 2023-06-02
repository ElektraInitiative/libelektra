find_package (Threads QUIET)
find_package (LibRt QUIET)

if (APPLE)

	# macOS does not provide pthread_barrier_t and all related functions are not to be found. Thus we exclude the race test from macOS.
	remove_tool (race "race tool does not compile under macOS")
else ()
	if (NOT LIBRT_FOUND)
		remove_tool (race "race tool requires librt support")
		return ()
	endif ()

	add_executable (race race.c)
	tool_link_elektra (race)
	target_link_libraries (race ${CMAKE_THREAD_LIBS_INIT})
	target_link_libraries (race ${LIBRT_LIBRARIES})

	install (
		TARGETS race
		DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
		COMPONENT elektra-tests)
endif ()
