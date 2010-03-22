# For static inclusion of libraries we need a list
# of symbols to make the static runtime linker work.
#
# For that there is the program exportsymbols which
# generates the sourcefiles exported_symbols.h
# and exported_symbols.c containing an array of
# all symbols.
#
# The program needs a list of all backends as
# argument.
#
# Symbols of libelektratools will be treaded specially.
add_executable (exportsymbols exportsymbols.c)

get_target_property (EXE_LOC exportsymbols LOCATION)

message ("is here: ${EXE_LOC}")

add_custom_command (
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.h
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS ${BACKENDS})
add_custom_command (
		OUTPUT ${PROJECT_BINARY_DIR}/exported_symbols.c
		DEPENDS exportsymbols
		COMMAND ${EXE_LOC}
		ARGS ${BACKENDS})

# Include the shared header files of the elektra project
include(LibAddMacros)
add_headers(HDR_FILES)

# include the directories of all libraries for the static or
# full-shared build
get_property (elektra_INCLUDES GLOBAL PROPERTY elektra_INCLUDES)
include_directories (${elektra_INCLUDES})

#now add all source files of this folder
file (GLOB SRC_FILES *.c)

#now add all source files of other folders
get_property (elektra_SRCS GLOBAL PROPERTY elektra_SRCS)
list (APPEND SRC_FILES ${elektra_SRCS})

#
#Elektra dynamic build with modules
#
set (SOURCES ${SRC_FILES} ${HDR_FILES})

add_library (elektra SHARED ${SOURCES})

get_property (elektra_LIBRARIES GLOBAL PROPERTY elektra_LIBRARIES)
target_link_libraries (elektra ${elektra_LIBRARIES})

set_target_properties (elektra PROPERTIES
		COMPILE_DEFINITIONS HAVE_CONFIG_H)
set_target_properties (elektra PROPERTIES
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION})

install_targets (/lib elektra)

#for the next two builds, append some more sources
get_property (elektra-full_SRCS GLOBAL PROPERTY elektra-full_SRCS)
list (APPEND SOURCES ${elektra-full_SRCS})
list (APPEND SOURCES "${PROJECT_BINARY_DIR}/exported_symbols.c" "${PROJECT_BINARY_DIR}/exported_symbols.h")

#and get all libraries to link against
get_property (elektra-full_LIBRARIES GLOBAL PROPERTY elektra-full_LIBRARIES)

#
#Elektra dynamic build with everything included
#
add_library (elektra-full SHARED ${SOURCES})

target_link_libraries (elektra-full ${elektra-full_LIBRARIES})

set_target_properties (elektra-full PROPERTIES
		COMPILE_DEFINITIONS "HAVE_CONFIG_H;ELEKTRA_STATIC")
set_target_properties (elektra-full PROPERTIES
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION})

install_targets (/lib elektra-full)

#
#Elektra static build with everything included
#
add_library (elektra-static STATIC ${SOURCES})

target_link_libraries (elektra-static ${elektra-full_LIBRARIES})

set_target_properties (elektra-static PROPERTIES
		COMPILE_DEFINITIONS "HAVE_CONFIG_H;ELEKTRA_STATIC")
set_target_properties (elektra-static PROPERTIES
		VERSION ${KDB_VERSION} SOVERSION ${SO_VERSION})

install_targets (/lib elektra-static)
