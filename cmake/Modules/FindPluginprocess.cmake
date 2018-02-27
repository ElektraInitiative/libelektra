# Find the dependencies of libpluginprocess to determine if plugins
# which depend on it can be included or not.
#
#  HAVE_MKFIFO					- True if mkfifo is available on the platform
#  HAVE_FORK					- True if fork is available on the platform
#  HAVE_PLUGINPROCESS			- True if the pluginprocess library can be built
#  PLUGINPROCESS_NOTFOUND_INFO	- A string describing which pluginprocess dependency is missing
#
include(CheckFunctionExists)

check_function_exists (mkfifo HAVE_MKFIFO)
check_function_exists (fork HAVE_FORK)

if (HAVE_MKFIFO)
if (HAVE_FORK)
	set (PLUGINPROCESS_FOUND 1)
else (HAVE_FORK)
	set (PLUGINPROCESS_NOTFOUND_INFO
		"fork does not exist on the target platform, excluding pluginprocess library")
endif(HAVE_FORK)
else(HAVE_MKFIFO)
	message (PLUGINPROCESS_NOTFOUND_INFO
		"mkfifo does not exist on the target platform, excluding pluginprocess library")
endif(HAVE_MKFIFO)

mark_as_advanced (
	HAVE_MKFIFO
	HAVE_FORK
	PLUGINPROCESS_FOUND
	PLUGINPROCESS_NOTFOUND_INFO
)
