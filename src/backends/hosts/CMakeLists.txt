set (SOURCES hosts.c hosts.h)

include_directories("${PROJECT_BINARY_DIR}/src/include")
add_library(elektra-hosts MODULE ${SOURCES})
