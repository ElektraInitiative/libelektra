cmake_minimum_required (VERSION 2.6)

project (Elektra)

set (KDB_VERSION_MAJOR 0)
set (KDB_VERSION_MINOR 8)
set (KDB_VERSION_MICRO 0)

set (KDB_VERSION "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}.${KDB_VERSION_MICRO}")
set (KDB_VERSION_MAJORMINOR "${KDB_VERSION_MAJOR}.${KDB_VERSION_MINOR}")

set (PACKAGE_URL "http://www.libelektra.org/")

set (KDB_DB_SYSTEM "/etc")

add_subdirectory(src)
