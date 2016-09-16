# Defines:
#
# LibGit2_FOUND
# LibGit2_INCLUDE_DIR
# LibGit2_LIBRARIES

find_path (LibGit2_INCLUDE_DIR NAMES git2.h)
find_library (LibGit2_LIBRARIES NAMES git2)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (LibGit2
		FOUND_VAR LibGit2_FOUND
		REQUIRED_VARS LibGit2_LIBRARIES LibGit2_INCLUDE_DIR
		)

mark_as_advanced (LibGit2_INCLUDE_DIR LibGit2_LIBRARIES)
