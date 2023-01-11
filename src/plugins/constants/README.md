- infos = All information you want to know
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/info
- infos/needs =
- infos/recommends =
- infos/placements = setstorage getstorage
- infos/status = maintained nodep
- infos/features/storage = read limited
- infos/description = includes constants information into kdb

## Introduction

Includes constants information into the key database. The constants are defined during CMake build.

The plugin is readonly.

## Usage

To mount it, use

```sh
kdb mount -R noresolver none user:/tests/constants constants
```

To list all constants, use:

```sh
kdb ls user:/tests/constants
#> user:/tests/constants
#> user:/tests/constants/cmake
#> user:/tests/constants/cmake/BINDINGS
#> user:/tests/constants/cmake/BUILD_FULL
#> user:/tests/constants/cmake/BUILD_SHARED
#> user:/tests/constants/cmake/BUILD_STATIC
#> user:/tests/constants/cmake/BUILTIN_DATA_FOLDER
#> user:/tests/constants/cmake/BUILTIN_EXEC_FOLDER
#> user:/tests/constants/cmake/BUILTIN_PLUGIN_FOLDER
#> user:/tests/constants/cmake/CMAKE_INSTALL_PREFIX
#> user:/tests/constants/cmake/ENABLE_ASAN
#> user:/tests/constants/cmake/ENABLE_DEBUG
#> user:/tests/constants/cmake/ENABLE_LOGGER
#> user:/tests/constants/cmake/GTEST_ROOT
#> user:/tests/constants/cmake/KDB_DB_DIR
#> user:/tests/constants/cmake/KDB_DB_FILE
#> user:/tests/constants/cmake/KDB_DB_HOME
#> user:/tests/constants/cmake/KDB_DB_INIT
#> user:/tests/constants/cmake/KDB_DB_SPEC
#> user:/tests/constants/cmake/KDB_DB_SYSTEM
#> user:/tests/constants/cmake/KDB_DB_USER
#> user:/tests/constants/cmake/KDB_DEFAULT_RESOLVER
#> user:/tests/constants/cmake/KDB_DEFAULT_STORAGE
#> user:/tests/constants/cmake/LIB_SUFFIX
#> user:/tests/constants/cmake/PLUGINS
#> user:/tests/constants/cmake/TARGET_CMAKE_FOLDER
#> user:/tests/constants/cmake/TARGET_DOCUMENTATION_HTML_FOLDER
#> user:/tests/constants/cmake/TARGET_DOCUMENTATION_LATEX_FOLDER
#> user:/tests/constants/cmake/TARGET_DOCUMENTATION_MAN_FOLDER
#> user:/tests/constants/cmake/TARGET_DOCUMENTATION_TEXT_FOLDER
#> user:/tests/constants/cmake/TARGET_INCLUDE_FOLDER
#> user:/tests/constants/cmake/TARGET_PKGCONFIG_FOLDER
#> user:/tests/constants/cmake/TARGET_PLUGIN_FOLDER
#> user:/tests/constants/cmake/TARGET_TEMPLATE_FOLDER
#> user:/tests/constants/cmake/TARGET_TEST_DATA_FOLDER
#> user:/tests/constants/cmake/TARGET_TOOL_DATA_FOLDER
#> user:/tests/constants/cmake/TARGET_TOOL_EXEC_FOLDER
#> user:/tests/constants/cmake/TOOLS
#> user:/tests/constants/compiler
#> user:/tests/constants/compiler/c_flags
#> user:/tests/constants/compiler/coverage
#> user:/tests/constants/compiler/cxx_flags
#> user:/tests/constants/compiler/id
#> user:/tests/constants/compiler/pic_flags
#> user:/tests/constants/compiler/static_flags
#> user:/tests/constants/macros
#> user:/tests/constants/macros/KDB_DIR_MODE
#> user:/tests/constants/macros/KDB_FILE_MODE
#> user:/tests/constants/macros/KDB_MAX_PATH_LENGTH
#> user:/tests/constants/macros/KDB_PATH_ESCAPE
#> user:/tests/constants/macros/KDB_PATH_SEPARATOR
#> user:/tests/constants/package
#> user:/tests/constants/package/cflags
#> user:/tests/constants/package/includedir
#> user:/tests/constants/package/libdir
#> user:/tests/constants/package/libs
#> user:/tests/constants/package/plugindir
#> user:/tests/constants/package/prefix
#> user:/tests/constants/package/templatedir
#> user:/tests/constants/package/tool_execdir
#> user:/tests/constants/version
#> user:/tests/constants/version/KDB_VERSION_MAJOR
#> user:/tests/constants/version/KDB_VERSION_MINOR
#> user:/tests/constants/version/KDB_VERSION_PATCH
#> user:/tests/constants/version/SO_VERSION
#> user:/tests/constants/version/SO_VERSION_GETENV
#> user:/tests/constants/version/SO_VERSION_TOOLS
#> user:/tests/constants/version/version/KDB_VERSION
```

You can unmount the plugin with:

```sh
kdb umount user:/tests/constants
```
