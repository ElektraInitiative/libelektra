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

find_path (
	XFCONF_INCLUDE_DIR
	PATHS /usr/include /usr/local/include
	PATH_SUFFIXES xfce4/xfconf-0)
find_library (
	XFCONF_LIBRARY
	NAMES xfconf-0
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

# Handle the QUIETLY and REQUIRED arguments and set XFCONF_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Xfconf DEFAULT_MSG XFCONF_LIBRARY XFCONF_INCLUDE_DIR)

mark_as_advanced (XFCONF_LIBRARY XFCONF_INCLUDE_DIR)
