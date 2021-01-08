#!/bin/sh

set -ex

ELEKTRA_PLUGINS='ALL;mozprefs;multifile;-gitresolver;jni;-ruby;-haskell;yamlcpp;toml'
ELEKTRA_TOOLS='ALL'
ELEKTRA_BINDINGS='cpp;lua;python;jna;INTERCEPT'

PACKAGE_REVISION=${1:-1}
DIST_NAME=${2:-$(grep "^NAME=" /etc/os-release | awk -F= {' print $2'} | sed 's/\"//g')}

ARCHITECTURE=$(uname -m)
if [ "$ARCHITECTURE" = "x86_64" ]; then
	LUA_LIB_SUFFIX="64"
else
	LUA_LIB_SUFFIX=""
fi

LUA_VERSION=$(lua -v | grep -Po '(?<=Lua )\d.\d')

echo "DIST: $DIST_NAME"

CMAKE_ARGS_BASE="-DTARGET_PLUGIN_FOLDER='elektra4' \
    -DBUILD_STATIC=OFF \
    -DGTEST_ROOT='/usr/src/gtest' \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DPLUGINS=$ELEKTRA_PLUGINS \
    -DTOOLS=$ELEKTRA_TOOLS \
    -DBINDINGS=$ELEKTRA_BINDINGS \
    -DKDB_DB_SYSTEM:PATH=/etc/kdb \
    -DKDB_DB_HOME:PATH=/home \
    -DKDB_DB_USER:PATH=.config \
    -DINSTALL_DOCUMENTATION=ON \
    -DPYTHON_EXECUTABLE:PATH=/usr/bin/python3 \
    -DENABLE_TESTING=ON \
    -DINSTALL_TESTING=ON \
    -DENABLE_KDB_TESTING=OFF \
    -DINSTALL_SYSTEM_FILES=ON \
    -DBUILD_PDF=OFF \
    -DBUILD_FULL=ON \
    -DTARGET_DOCUMENTATION_HTML_FOLDER=share/doc/libelektra-doc/html \
    -DTARGET_TEST_DATA_FOLDER=share/libelektra-test/test-data"

if [ "$DIST_NAME" = "Fedora" ]; then

	CMAKE_ARGS_SPECIFIC="-DTARGET_LUA_CMOD_FOLDER=lib$LUA_LIB_SUFFIX/lua/$LUA_VERSION \
    -DTARGET_LUA_LMOD_FOLDER=share/lua/$LUA_VERSION \
    -DSWIG_EXECUTABLE=/usr/bin/swig \
    -DCPACK_RPM_PACKAGE_RELEASE=$PACKAGE_REVISION"
else

	PY3VER=$(py3versions -d -v)
	# workaround for hardening flags
	CPPFLAGS=$(dpkg-buildflags --get CPPFLAGS)
	CFLAGS=$(dpkg-buildflags --get CFLAGS)
	CXXFLAGS=$(dpkg-buildflags --get CXXFLAGS)
	LDFLAGS=$(dpkg-buildflags --get LDFLAGS)

	CMAKE_ARGS_SPECIFIC="-DTARGET_LUA_CMOD_FOLDER=lib/lua/$LUA_VERSION \
    -DTARGET_LUA_LMOD_FOLDER=share/lua/$LUA_VERSION \
    -DSWIG_EXECUTABLE=/usr/bin/swig3.0 \
    -DCMAKE_C_FLAGS=$CFLAGS \
    -DCMAKE_CXX_FLAGS=$CXXFLAGS \
    -DCMAKE_EXE_LINKER_FLAGS=$LDFLAGS \
    -DCMAKE_MODULE_LINKER_FLAGS=$LDFLAGS \
    -DCMAKE_SHARED_LINKER_FLAGS=$LDFLAGS \
    -DCPACK_DEBIAN_PACKAGE_RELEASE=$PACKAGE_REVISION"
fi

cmake $CMAKE_ARGS_BASE $CMAKE_ARGS_SPECIFIC ..
LD_LIBRARY_PATH=$(pwd)/lib:${LD_LIBRARY_PATH} make package
