# ~~~
# Find the libxfconf includes and library
#
#  XFCONF_INCLUDE_DIR  - Where to find libxfconf include sub-directory.
#  XFCONF_LIBRARY      - Path to libxfconf library.
#  XFCONF_FOUND        - True if libxfconf found.
# ~~~

if (XFCONF_INCLUDE_DIR) # Already in cache, be silent.
	set (XFCONF_FIND_QUIETLY TRUE)
endif (XFCONF_INCLUDE_DIR)

find_package (PkgConfig QUIET)

if (NOT PkgConfig_FOUND)
	message ("PkgConfig is required to locate xfconf")
	return ()
endif ()

pkg_search_module (GLIB QUIET glib-2.0)

if (NOT GLIB_FOUND)
	message ("glib-2.0 is required to xfconf")
	return ()
endif ()

pkg_search_module (GOBJECT QUIET gobject-2.0)

if (NOT GOBJECT_FOUND)
	message ("gobject-2.0 is required to xfconf")
	return ()
endif ()

find_path (
	XFCONF_INCLUDE_DIR
	NAMES xfconf
	PATHS /usr/include/xfce4/xfconf-0 /usr/local/include/xfce4/xfconf-0)

if (XFCONF_INCLUDE_DIR-NOTFOUND)
	message ("xfconf headers cannot be found")
	return ()
endif ()

find_library (
	XFCONF_LIBRARY
	NAMES xfconf-0
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

if (XFCONF_LIBRARY-NOTFOUND)
	message ("xfconf library cannot be found")
	return ()
endif ()

find_library (
	GLIB_LIBRARY
	NAMES glib-2.0
	PATHS "${GLIB_LIBDIR}")

if (GLIB_LIBRARY-NOTFOUND)
	message ("glib library cannot be found")
	return ()
endif ()

find_library (
	GOBJECT_LIBRARY
	NAMES gobject-2.0
	PATHS "${GOBJECT_LIBDIR}")

if (GOBJECT_LIBRARY-NOTFOUND)
	message ("gobject library cannot be found")
	return ()
endif ()

file (GLOB XFCONF_INCLUDE_DIRS "${XFCONF_INCLUDE_DIR}" "${GLIB_INCLUDEDIR}/glib-2.0" "${GLIB_LIBDIR}/glib-2.0/include")
file (GLOB XFCONF_LIBRARIES "${XFCONF_LIBRARY}" "${GLIB_LIBRARY}" "${GOBJECT_LIBRARY}")

# Handle the QUIETLY and REQUIRED arguments and set XFCONF_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Xfconf DEFAULT_MSG GLIB_LIBDIR GLIB_INCLUDEDIR XFCONF_LIBRARY XFCONF_INCLUDE_DIR)

mark_as_advanced (GLIB_LIBDIR GLIB_INCLUDEDIR XFCONF_LIBRARY XFCONF_INCLUDE_DIR)
