# ~~~
# Print a list of Elektra's dependencies
#
# (currently only essential deps, excluding CMake which is needed
# to run this script)
#
# == Usage ==
#
# On Debian/Ubuntu:
#
# apt-get install `cmake -DPLUGINS="ALL" -P cmake/PrintDependencies.cmake`
#
# On Apple:
#
# brew install `cmake -DPLUGINS="ALL" -P cmake/PrintDependencies.cmake`
# ~~~

execute_process (COMMAND lsb_release -is OUTPUT_VARIABLE LSB_DISTRIB)

if ("${LSB_DISTRIB}" MATCHES "Ubuntu|Debian")
	set (DEPS "build-essential")
elseif (APPLE)

endif ()

message (${DEPS})
