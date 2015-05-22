find_package(Threads)
find_library(LIBRT_LIBRARIES NAMES rt)

add_headers (HDR_FILES)

add_executable(race race.c)
tool_link_elektra(race)
target_link_libraries(race ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(race ${LIBRT_LIBRARIES})


install(TARGETS race DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
