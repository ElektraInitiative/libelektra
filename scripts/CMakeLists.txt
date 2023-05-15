include (ProcessorCount)

processorcount (PROCESSOR_COUNT)
if (PROCESSOR_COUNT EQUAL 0)
	set (PROCESSOR_COUNT 1)
endif (PROCESSOR_COUNT EQUAL 0)

set (CMAKE_MAKE_PROGRAM_BASENAME ${CMAKE_MAKE_PROGRAM})
string (REGEX REPLACE ".*/([^/]+)$" "\\1" CMAKE_MAKE_PROGRAM_BASENAME ${CMAKE_MAKE_PROGRAM_BASENAME})

install (
	PROGRAMS benchmark-createtree
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

install (
	PROGRAMS upgrade-bootstrap
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)

install (
	PROGRAMS kdb/mountpoint-info
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)
install (
	PROGRAMS check-env-dep
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-tests)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/make-source-package.in" "${CMAKE_CURRENT_BINARY_DIR}/make-source-package" @ONLY)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/configure-firefox.in" "${CMAKE_CURRENT_BINARY_DIR}/configure-firefox" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/configure-firefox"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin-extra)
add_subdirectory (ffconfig)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/change-resolver-symlink.in" "${CMAKE_CURRENT_BINARY_DIR}/change-resolver-symlink" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/change-resolver-symlink"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/change-storage-symlink.in" "${CMAKE_CURRENT_BINARY_DIR}/change-storage-symlink" @ONLY)
install (
	PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/change-storage-symlink"
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)

install (
	PROGRAMS stash
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)
install (
	PROGRAMS restore
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)

install (
	PROGRAMS reset
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)
install (
	PROGRAMS reset-elektra
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)
install (
	PROGRAMS umount-all
	DESTINATION ${TARGET_TOOL_EXEC_FOLDER}
	COMPONENT elektra-bin)

add_custom_target (
	source-package
	COMMAND "${CMAKE_CURRENT_BINARY_DIR}/make-source-package"
	COMMENT "Make a elektra source package by using git"
	VERBATIM
	WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/randoop/randoop.in" "${CMAKE_CURRENT_BINARY_DIR}/randoop/randoop" @ONLY)
file (COPY "${CMAKE_CURRENT_SOURCE_DIR}/randoop/randoopClasses.txt" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/randoop/")

add_subdirectory (dev)
add_subdirectory (kdb)
add_subdirectory (admin)
add_subdirectory (completion)
add_subdirectory (xfconf)
