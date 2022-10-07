# ~~~
# Defines:
#
# LibGit2_FOUND
# LibGit2_INCLUDE_DIR
# LibGit2_LIBRARIES
# LibGit2_VERSION
# ~~~

find_path (LibGit2_INCLUDE_DIR NAMES git2.h)
find_library (LibGit2_LIBRARIES NAMES git2)

set (LibGit2_VERSION 0)
find_file (LibGit2_VERSION_HEADER NAMES git2/version.h)
if (NOT LibGit2_VERSION_HEADER STREQUAL LibGit2_VERSION_HEADER-NOTFOUND)
	file (
		STRINGS ${LibGit2_VERSION_HEADER} LibGit2_VERSION
		REGEX "#define LIBGIT2_VERSION[ \t]+\".+\""
		LIMIT_COUNT 1)
	string (REGEX REPLACE ".*\"(.+)\"" "\\1" LibGit2_VERSION ${LibGit2_VERSION})
endif (NOT LibGit2_VERSION_HEADER STREQUAL LibGit2_VERSION_HEADER-NOTFOUND)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (
	LibGit2
	REQUIRED_VARS LibGit2_LIBRARIES LibGit2_INCLUDE_DIR
	VERSION_VAR LibGit2_VERSION)

mark_as_advanced (LibGit2_INCLUDE_DIR LibGit2_LIBRARIES)

set (LibGit2_INCLUDE_DIRS ${LibGit2_INCLUDE_DIR})
