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

find_package (PkgConfig REQUIRED)
pkg_search_module (GLIB REQUIRED glib-2.0)
pkg_search_module (GOBJECT REQUIRED gobject-2.0)

find_path (
	XFCONF_INCLUDE_DIR
	NAMES xfconf
	PATHS /usr/include/xfce4/xfconf-0 /usr/local/include/xfce4/xfconf-0)

find_library (
	XFCONF_LIBRARY
	NAMES xfconf-0
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

find_library (
	GLIB_LIBRARY
	NAMES glib-2.0
	PATHS "${GLIB_LIBDIR}")

find_library (
	GOBJECT_LIBRARY
	NAMES gobject-2.0
	PATHS "${GOBJECT_LIBDIR}")

file (GLOB XFCONF_INCLUDE_DIRS "${XFCONF_INCLUDE_DIR}" "${GLIB_INCLUDEDIR}/glib-2.0" "${GLIB_LIBDIR}/glib-2.0/include")
file (GLOB XFCONF_LIBRARIES "${XFCONF_LIBRARY}" "${GLIB_LIBRARY}" "${GOBJECT_LIBRARY}")

# Handle the QUIETLY and REQUIRED arguments and set XFCONF_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Xfconf DEFAULT_MSG GLIB_LIBDIR GLIB_INCLUDEDIR XFCONF_LIBRARY XFCONF_INCLUDE_DIR)

mark_as_advanced (GLIB_LIBDIR GLIB_INCLUDEDIR XFCONF_LIBRARY XFCONF_INCLUDE_DIR)
