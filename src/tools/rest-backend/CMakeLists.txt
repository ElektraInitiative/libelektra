find_package (CppCMS)
find_package (Boost 1.45 COMPONENTS system filesystem thread)

if (NOT CPPCMS_FOUND)
	remove_tool (rest-backend "CppCMS package not found")
elseif (NOT Boost_FOUND)
	remove_tool (rest-backend "Boost package not found")
else ()

    set (REST_AUTH_ENCRYPT_KEY "please_change" CACHE STRING "Encryption key to be used for auth tokens")
    set (REST_JWT_ISSUER "secret_issuer" CACHE STRING "The issuer to be used for JWT tokens, should be kept secret")
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

    file (GLOB rest-backend_TMPLS templates/*.tmpl)
    add_custom_command (
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/template_plain.cpp
        COMMAND cppcms_tmpl_cc
            ${rest-backend_TMPLS}
            -s plain
            -o ${CMAKE_CURRENT_BINARY_DIR}/template_plain.cpp
        DEPENDS ${rest-backend_TMPLS}
    )

    add_library (plain SHARED ${CMAKE_CURRENT_BINARY_DIR}/template_plain.cpp)
    target_link_libraries (plain ${Boost_LIBRARIES} ${CPPCMS_LIBRARY})

    file (GLOB rest-backend_SRCS *.cpp)

    add_library (tmp_rest-backend OBJECT ${rest-backend_SRCS})
    add_executable (rest-backend $<TARGET_OBJECTS:tmp_rest-backend>)
    target_link_libraries (rest-backend ${CPPCMS_LIBRARY} ${Boost_LIBRARIES})
    target_link_libraries (rest-backend elektra-full elektratools)

    install (TARGETS rest-backend DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
    install (TARGETS plain LIBRARY DESTINATION lib/elektra)
    install (FILES rest-backend-config.js DESTINATION ${TARGET_TOOL_EXEC_FOLDER})
    install (FILES run.sh DESTINATION ${TARGET_TOOL_EXEC_FOLDER} RENAME run-rest-backend.sh)
    install (FILES stop.sh DESTINATION ${TARGET_TOOL_EXEC_FOLDER} RENAME stop-rest-backend.sh)

    generate_manpage (kdb-rest-backend FILENAME ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
    
    if (ENABLE_TESTING)
	    add_subdirectory (tests)
    endif (ENABLE_TESTING)

endif ()
