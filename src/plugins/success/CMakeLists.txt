include(LibAddMacros)

set (SOURCES success.h success.c)
add_sources(elektra-full ${SOURCES})
add_headers(SOURCES)

add_library(elektra-success MODULE ${SOURCES})
#this needs posix, so it won't compile with c99 standard
#the last -std will be used, so it should be no problem
#that this is only added here
#set_target_properties (elektra-success PROPERTIES COMPILE_FLAGS "-std=gnu99")
install (TARGETS elektra-success DESTINATION lib/elektra)
