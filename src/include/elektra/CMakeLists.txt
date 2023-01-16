configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdbtypes.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdbtypes.h")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdb.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdb.h")

configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/kdbversion.h.in" "${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h")

install (
	FILES error.h
	      conversion.h
	      types.h
	      kdbextension.h
	      kdbmeta.h
	      kdbease.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdbtypes.h"
	      kdbhelper.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdb.h"
	      kdbmodule.h
	      kdbos.h
	      kdbopts.h
	      kdbplugin.h
	      kdbpluginprocess.h
	      kdbprivate.h
	      kdbinvoke.h
	      kdbutility.h
	      kdbio.h
	      kdbnotification.h
	      kdbglobbing.h
	      "${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h"
	      elektra.h
	      kdbendian.h
	      kdbmacros.h
	      kdbmerge.h
	      kdbgopts.h
	DESTINATION include/${TARGET_INCLUDE_FOLDER}/elektra
	COMPONENT libelektra-dev)

add_custom_target (elektra_headers ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/kdb.h ${CMAKE_CURRENT_BINARY_DIR}/kdbversion.h)
