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

# xfconf also requires glib-2.0
find_path (
	GLIB_INCLUDE_DIR
	NAMES glib.h
	PATHS ${Glib_PKGCONF_INCLUDE_DIRS}
	PATH_SUFFIXES glib-2.0)

find_library (
	GLIB_LIBRARY
	NAMES glib-2.0
	PATHS ${Glib_PKGCONF_LIBRARY_DIRS})

find_path (
	GLIBCONFIG_INCLUDE_DIR
	NAMES glibconfig.h
	PATHS ${Glib_PKGCONF_INCLUDE_DIRS} /usr
	PATH_SUFFIXES lib/glib-2.0/include)

find_path (
	XFCONF_INCLUDE_DIR
	NAMES xfconf
	PATHS /usr/include/xfce4/xfconf-0 /usr/local/include/xfce4/xfconf-0)
find_library (
	XFCONF_LIBRARY
	NAMES xfconf-0
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

# Handle the QUIETLY and REQUIRED arguments and set XFCONF_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	Xfconf
	DEFAULT_MSG
	GLIBCONFIG_INCLUDE_DIR
	GLIB_LIBRARY
	GLIB_INCLUDE_DIR
	XFCONF_LIBRARY
	XFCONF_INCLUDE_DIR)

mark_as_advanced (GLIBCONFIG_INCLUDE_DIR GLIB_LIBRARY GLIB_INCLUDE_DIR XFCONF_LIBRARY XFCONF_INCLUDE_DIR)
