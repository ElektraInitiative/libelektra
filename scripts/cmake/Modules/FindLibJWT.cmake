# ~~~
# Find the libjwt includes and library
#
#  LIBJWT_INCLUDE_DIR  - Where to find libjwt include sub-directory.
#  LIBJWT_LIBRARY      - Path to libjwt library.
#  LIBJWT_FOUND        - True if libjwt found.
# ~~~

if (LIBJWT_INCLUDE_DIR)
	set (LIBJWT_FIND_QUIETLY TRUE) # Already in cache, be silent.
endif (LIBJWT_INCLUDE_DIR)

find_path (
	LIBJWT_INCLUDE_DIR jwt.h
	PATHS /usr/include /usr/local/include
	PATH_SUFFIXES jwt)
find_library (
	LIBJWT_LIBRARY
	NAMES jwt
	PATHS /usr/lib /usr/lib64 /usr/local/lib)

# Handle the QUIETLY and REQUIRED arguments and set LIBJWT_FOUND to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (LibJWT DEFAULT_MSG LIBJWT_LIBRARY LIBJWT_INCLUDE_DIR)

mark_as_advanced (LIBJWT_LIBRARY LIBJWT_INCLUDE_DIR)
