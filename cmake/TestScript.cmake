set (CTEST_SOURCE_DIRECTORY /home/markus/Projekte/Elektra/current)
set (CTEST_BINARY_DIRECTORY /home/markus/Projekte/Elektra/current/build)
set (CTEST_COMMAND ctest)
set ($ENV{srcdir} /home/markus/Projekte/Elektra/current/build/tests)

ctest_memcheck ()
