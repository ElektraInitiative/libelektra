- infos = All information you want to know
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/info
- infos/needs =
- infos/recommends =
- infos/placements = setstorage getstorage
- infos/status = maintained nodep libc readonly limited nodoc concept
- infos/description = includes constants information into kdb

## Introduction

Includes constants information into the key database. The constants are defined during CMake build.

The plugin is readonly.

## Usage

To mount it, use

```sh
kdb mount -R noresolver none user/examples/constants constants
```

To list all constants, use:

```sh
kdb ls user/examples/constants
#> user/examples/constants
#> user/examples/constants/cmake
#> user/examples/constants/cmake/BINDINGS
#> user/examples/constants/cmake/BUILD_FULL
#> user/examples/constants/cmake/BUILD_SHARED
#> user/examples/constants/cmake/BUILD_STATIC
#> user/examples/constants/cmake/BUILTIN_DATA_FOLDER
#> user/examples/constants/cmake/BUILTIN_EXEC_FOLDER
#> user/examples/constants/cmake/BUILTIN_PLUGIN_FOLDER
#> user/examples/constants/cmake/CMAKE_INSTALL_PREFIX
#> user/examples/constants/cmake/ENABLE_DEBUG
#> user/examples/constants/cmake/ENABLE_LOGGER
#> user/examples/constants/cmake/GTEST_ROOT
#> user/examples/constants/cmake/KDB_DB_DIR
#> user/examples/constants/cmake/KDB_DB_FILE
#> user/examples/constants/cmake/KDB_DB_HOME
#> user/examples/constants/cmake/KDB_DB_INIT
#> user/examples/constants/cmake/KDB_DB_SPEC
#> user/examples/constants/cmake/KDB_DB_SYSTEM
#> user/examples/constants/cmake/KDB_DB_USER
#> user/examples/constants/cmake/KDB_DEFAULT_RESOLVER
#> user/examples/constants/cmake/KDB_DEFAULT_STORAGE
#> user/examples/constants/cmake/LIB_SUFFIX
#> user/examples/constants/cmake/PLUGINS
#> user/examples/constants/cmake/TARGET_CMAKE_FOLDER
#> user/examples/constants/cmake/TARGET_DOCUMENTATION_HTML_FOLDER
#> user/examples/constants/cmake/TARGET_DOCUMENTATION_LATEX_FOLDER
#> user/examples/constants/cmake/TARGET_DOCUMENTATION_MAN_FOLDER
#> user/examples/constants/cmake/TARGET_DOCUMENTATION_TEXT_FOLDER
#> user/examples/constants/cmake/TARGET_INCLUDE_FOLDER
#> user/examples/constants/cmake/TARGET_PKGCONFIG_FOLDER
#> user/examples/constants/cmake/TARGET_PLUGIN_FOLDER
#> user/examples/constants/cmake/TARGET_TEMPLATE_FOLDER
#> user/examples/constants/cmake/TARGET_TEST_DATA_FOLDER
#> user/examples/constants/cmake/TARGET_TOOL_DATA_FOLDER
#> user/examples/constants/cmake/TARGET_TOOL_EXEC_FOLDER
#> user/examples/constants/cmake/TOOLS
#> user/examples/constants/compiler
#> user/examples/constants/compiler/c_flags
#> user/examples/constants/compiler/coverage
#> user/examples/constants/compiler/cxx_flags
#> user/examples/constants/compiler/id
#> user/examples/constants/compiler/pic_flags
#> user/examples/constants/compiler/static_flags
#> user/examples/constants/macros
#> user/examples/constants/macros/KDB_DIR_MODE
#> user/examples/constants/macros/KDB_FILE_MODE
#> user/examples/constants/macros/KDB_MAX_PATH_LENGTH
#> user/examples/constants/macros/KDB_PATH_ESCAPE
#> user/examples/constants/macros/KDB_PATH_SEPARATOR
#> user/examples/constants/package
#> user/examples/constants/package/cflags
#> user/examples/constants/package/includedir
#> user/examples/constants/package/libdir
#> user/examples/constants/package/libs
#> user/examples/constants/package/plugindir
#> user/examples/constants/package/prefix
#> user/examples/constants/package/templatedir
#> user/examples/constants/package/tool_execdir
#> user/examples/constants/version
#> user/examples/constants/version/KDB_VERSION_MAJOR
#> user/examples/constants/version/KDB_VERSION_MICRO
#> user/examples/constants/version/KDB_VERSION_MINOR
#> user/examples/constants/version/SO_VERSION
#> user/examples/constants/version/SO_VERSION_GETENV
#> user/examples/constants/version/SO_VERSION_TOOLS
#> user/examples/constants/version/version/KDB_VERSION
```

You can unmount the plugin with:

```sh
kdb umount user/examples/constants
```
