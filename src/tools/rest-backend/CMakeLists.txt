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

    set (REST_AUTH_ENCRYPT_KEY "please_change_to_secret_key" CACHE STRING "Encryption key to be used for auth tokens")
    set (REST_JWT_ISSUER "additional_secret_issuer" CACHE STRING "The issuer to be used for JWT tokens, should be kept secret")
    set (REST_REPOSITORY_CONFIGS "dir/configs" CACHE STRING "Path to be used to store configuration snippets")
    set (REST_REPOSITORY_USERS "dir/users" CACHE STRING "Path to be used to store user information")

    configure_file (
        "${CMAKE_CURRENT_SOURCE_DIR}/config.hpp.in"
        "${CMAKE_CURRENT_BINARY_DIR}/config.hpp"
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

    add_library (tmp_${tool} OBJECT ${rest-backend_SRCS})
    add_executable (${tool} $<TARGET_OBJECTS:tmp_${tool}>)
    target_link_libraries (${tool} ${CPPCMS_LIBRARY} ${Boost_LIBRARIES} ${LIBJWT_LIBRARY} ${OPENSSL_LIBRARIES})
    target_link_libraries (${tool} elektra-full elektratools)

    install (TARGETS ${tool} DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
    install (FILES rest-backend-config.json DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
    install (FILES run.sh DESTINATION ${TARGET_TOOL_EXEC_FOLDER} RENAME run-${tool}.sh)
    install (FILES stop.sh DESTINATION ${TARGET_TOOL_EXEC_FOLDER} RENAME stop-${tool}.sh)

    generate_manpage (kdb-${tool} FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
    
    if (ENABLE_TESTING)
	    add_subdirectory (tests)
    endif (ENABLE_TESTING)

endif ()
