set (SOURCES filesys.h filesys.c)

include_directories("${PROJECT_BINARY_DIR}/src/include")
add_library(elektra-filesys MODULE ${SOURCES})
install_targets (/lib elektra-filesys)
