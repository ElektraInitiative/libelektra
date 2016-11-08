include (LibAddMacros)

find_package (MySqlCppConn)

if (NOT MYSQLCPPCONN_FOUND)
    message (STATUS "MySQL Connector library for C++ not found, cannot build benchmark")
else ()

    include_directories (${CMAKE_CURRENT_BINARY_DIR})
    include_directories (${MYSQLCPPCONN_INCLUDE_DIR})

    add_headers (HDR_FILES)
    add_cppheaders (HDR_FILES)
    add_toolheaders (HDR_FILES)

    macro (do_benchmark source other_sources libraries)
        include_directories (${CMAKE_CURRENT_SOURCE_DIR})
        set (SOURCES ${HDR_FILES} ${other_sources} ${source}.cpp)
        add_executable (${source} ${SOURCES})
        add_dependencies (${source} kdberrors_generated)
		target_link_libraries (${source} ${libraries})

#        set_target_properties (${source} PROPERTIES
#                COMPILE_DEFINITIONS HAVE_KDBCONFIG_H)
    endmacro (do_benchmark)

    set (mysql_other_sources testdata.cpp testdata.hpp timer.hpp ../service_convert.cpp)
    set (mysql_libraries ${MYSQLCPPCONN_LIBRARY} ${Boost_LIBRARIES} elektra-full elektratools)
    do_benchmark (benchmark_kdbrest_mysql "${mysql_other_sources}" "${mysql_libraries}")

    file (GLOB kdbrest_HDRS ../*.hpp)
    set (kdbrest_SRCS ../service_storage.cpp ../service_search.cpp)
    set (elektra_other_sources testdata.cpp testdata.hpp timer.hpp ${kdbrest_HDRS} ${kdbrest_SRCS})
    set (elektra_libraries ${Boost_LIBRARIES} elektra-full elektratools)
    do_benchmark (benchmark_kdbrest_elektra "${elektra_other_sources}" "${elektra_libraries}")

endif ()