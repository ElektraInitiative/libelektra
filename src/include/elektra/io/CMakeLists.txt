# TODO: move to src/include/CMakeLists.txt ?

check_binding_included ("io_uv" IO_UV_INCLUDED SUBDIRECTORY "io/uv" SILENT)
if (IO_UV_INCLUDED)
	install (
		FILES uv.h
		DESTINATION include/${TARGET_INCLUDE_FOLDER}/kdbio
		COMPONENT libelektra-dev)
endif ()

check_binding_included ("io_glib" IO_GLIB_INCLUDED SUBDIRECTORY "io/glib" SILENT)
if (IO_GLIB_INCLUDED)
	install (
		FILES glib.h
		DESTINATION include/${TARGET_INCLUDE_FOLDER}/kdbio
		COMPONENT libelektra-dev)
endif ()

check_binding_included ("io_ev" IO_EV_INCLUDED SUBDIRECTORY "io/ev" SILENT)
if (IO_EV_INCLUDED)
	install (
		FILES ev.h
		DESTINATION include/${TARGET_INCLUDE_FOLDER}/kdbio
		COMPONENT libelektra-dev)
endif ()
