include (LibAddLib)

set (
	SUBDIRS
	loader
	ease
	globbing
	meta
	plugin
	pluginprocess
	utility
	io
	invoke
	notification
	highlevel
	merge
	record
	opts)

# The subdirectory for LibElektra must be the last entry!
list (APPEND SUBDIRS elektra)

set (SYM_MAPS "")
foreach (subdir ${SUBDIRS})
	list (APPEND SYM_MAPS "${subdir}/symbols.map")
endforeach (subdir)

# tools has special symbol handling
list (INSERT SUBDIRS 0 tools)

add_custom_command (
	OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	COMMAND awk 1 ${SYM_MAPS} | awk -v vfile=versions.def -f version_gen.awk > "${CMAKE_CURRENT_BINARY_DIR}/symbols.map"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
	DEPENDS version_gen.awk versions.def ${SYM_MAPS}
	COMMENT "Creating version script"
	VERBATIM)
add_custom_target (generate_version_script DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/symbols.map")

foreach (subdir ${SUBDIRS})
	add_subdirectory ("${subdir}")
endforeach (subdir)

generate_man_page (
	elektra-libs
	FILENAME
	"${CMAKE_CURRENT_SOURCE_DIR}/README.md"
	SECTION
	7
	COMPONENT
	elektra-doc)
