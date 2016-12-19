include (LibAddMacros)

find_package (MySqlCppConn)

if (NOT MYSQLCPPCONN_FOUND)
    message (STATUS "MySQL Connector library for C++ not found, cannot build benchmark")
else ()

    include_directories (${CMAKE_CURRENT_SOURCE_DIR})
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
    endmacro (do_benchmark)

    set (kdbrest_libs ${Boost_LIBRARIES} ${CPPCMS_LIBRARY} ${LIBJWT_LIBRARY} ${OPENSSL_LIBRARIES})
    set (mysql_other_sources testdata.cpp testdata.hpp timer.hpp $<TARGET_OBJECTS:tmp_${tool}>)
    set (mysql_libraries ${MYSQLCPPCONN_LIBRARY} ${kdbrest_libs} elektra-core elektratools)
    do_benchmark (benchmark_kdbrest_mysql "${mysql_other_sources}" "${mysql_libraries}")

    set (elektra_other_sources testdata.cpp testdata.hpp timer.hpp $<TARGET_OBJECTS:tmp_${tool}>)
    set (elektra_libraries ${kdbrest_libs} elektra-core elektratools)
    do_benchmark (benchmark_kdbrest_elektra "${elektra_other_sources}" "${elektra_libraries}")

endif ()