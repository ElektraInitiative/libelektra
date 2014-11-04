find_package(Threads)

add_headers (HDR_FILES)

add_executable(race race.c)
target_link_libraries(race ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(race elektra-static)

install(TARGETS race DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
