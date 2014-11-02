find_package(Threads)

add_executable(test_race test_race.c)
target_link_libraries(test_race ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(test_race elektra-static)
