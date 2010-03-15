cmake_minimum_required (VERSION 2.6)

project (Elektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 0)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")
set (KDB_VERSION_MAJORMINOR "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}")

set (PACKAGE_NAME "Elektra")
set (PACKAGE_URL "http://www.libelektra.org/")
set (PACKAGE_BUGREPORT "http://bugs.libelektra.org/")

set (BACKENDS filesys hosts fstab)

set (KDB_DB_SYSTEM "/etc")

set (DEBUG "1")
set (VERBOSE "0")

add_subdirectory(src)
