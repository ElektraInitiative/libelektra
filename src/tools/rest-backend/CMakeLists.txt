find_package (CppCMS)
find_package (Boost 1.45 COMPONENTS system filesystem thread)
find_package (LibJWT)
find_package (OpenSSL)

if (NOT CPPCMS_FOUND)
    remove_tool (${tool} "CppCMS package not found")
elseif (NOT Boost_FOUND)
    remove_tool (${tool} "Boost package not found")
elseif (NOT LIBJWT_FOUND)
    remove_tool (${tool} "LibJWT package not found")
elseif (NOT OPENSSL_FOUND)
    remove_tool (${tool} "OpenSSL package not found")
else ()

    string (REGEX REPLACE "[^a-zA-Z]" "" tool_name_simplified ${tool})
    set (config_root "/sw/elektra/${tool_name_simplified}/#0/")
    set (config_default_profile "current")

    set (install_directory ${TARGET_TOOL_DATA_FOLDER}/${tool})
    file (READ config-specification.ini configuration-specification)

    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/config.hpp.in"
        "${CMAKE_CURRENT_BINARY_DIR}/config.hpp"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/run.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/run-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/stop.sh"
        "${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}"
    )
    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
        "${CMAKE_CURRENT_BINARY_DIR}/README.md"
    )

    file (GLOB rest-backend_HDRS *.hpp)
    add_headers (rest-backend_HDRS)
    add_cppheaders (rest-backend_HDRS)
    add_toolheaders (rest-backend_HDRS)

    include_directories (${CMAKE_CURRENT_SOURCE_DIR})
    include_directories (${CMAKE_CURRENT_BINARY_DIR})
    include_directories (${CPPCMS_INCLUDE_DIR})
    include_directories (${Boost_INCLUDE_DIRS})
    include_directories (${LIBJWT_INCLUDE_DIR})
    include_directories (${OPENSSL_INCLUDE_DIR})

    file (GLOB rest-backend_SRCS *.cpp)
    file (GLOB main_SRC *main.cpp)
    list (REMOVE_ITEM rest-backend_SRCS ${main_SRC})

    add_library (tmp_${tool} OBJECT ${rest-backend_SRCS})
    add_library (tmp_${tool}_main OBJECT main.cpp)
    add_executable (${tool} $<TARGET_OBJECTS:tmp_${tool}> $<TARGET_OBJECTS:tmp_${tool}_main>)
    target_link_libraries (${tool} ${CPPCMS_LIBRARY} ${Boost_LIBRARIES} ${LIBJWT_LIBRARY} ${OPENSSL_LIBRARIES})
    target_link_libraries (${tool} elektra-full elektratools)

    install (TARGETS ${tool} DESTINATION ${install_directory})
    install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run-${tool} ${CMAKE_CURRENT_BINARY_DIR}/stop-${tool}
             DESTINATION ${TARGET_TOOL_EXEC_FOLDER})

    generate_manpage (kdb-run-${tool} FILENAME ${CMAKE_CURRENT_BINARY_DIR}/README.md)
    
    if (ENABLE_TESTING)
        add_subdirectory (tests)
    endif (ENABLE_TESTING)

    add_subdirectory (benchmarks)

endif ()
