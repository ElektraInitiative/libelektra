#!/bin/sh

ELEKTRA_PLUGINS='ALL;mozprefs;multifile;-gitresolver;-jni;-ruby;-haskell;-yamlcpp;toml'
ELEKTRA_TOOLS='ALL'
ELEKTRA_BINDINGS='cpp;lua;python;INTERCEPT'

# #workaround for hardening flags
# CPPFLAGS=$(dpkg-buildflags --get CPPFLAGS)
# CFLAGS=$(dpkg-buildflags --get CFLAGS $CPPFLAGS)
# CXXFLAGS=$(dpkg-buildflags --get CXXFLAGS) $(CPPFLAGS)
# LDFLAGS=$(dpkg-buildflags --get LDFLAGS)

PY3VER=$(py3versions -d -v)
#DEB_HOST_MULTIARCH=$(dpkg-architecture -qDEB_HOST_MULTIARCH)

CMAKE_ARGS="-DTARGET_PLUGIN_FOLDER='elektra4' \
  -DBUILD_STATIC=OFF \
  -DPython_ADDITIONAL_VERSIONS=$PY3VER \
  -DGTEST_ROOT='/usr/src/gtest' \
  -DPLUGINS=$ELEKTRA_PLUGINS \
  -DTOOLS=$ELEKTRA_TOOLS \
  -DBINDINGS=$ELEKTRA_BINDINGS \
  -DINSTALL_DOCUMENTATION=ON \
  -DSWIG_EXECUTABLE=/usr/bin/swig3.0 \
  -DPYTHON_EXECUTABLE:PATH=/usr/bin/python3 \
  -DENABLE_TESTING=ON \
  -DINSTALL_TESTING=ON \
  -DENABLE_KDB_TESTING=OFF \
  -DINSTALL_SYSTEM_FILES=ON \
  -DBUILD_PDF=OFF \
  -DBUILD_FULL=ON"
  
  # \
  #-DTARGET_TEST_DATA_FOLDER=share/libelektra-test/test-data"

# DEBUG pls remove 
cpu_count=`nproc --all`
export MAKEFLAGS="-j$(($cpu_count+2)) -l$(($cpu_count*2))"
# end debug
cmake $CMAKE_ARGS ..
LD_LIBRARY_PATH=/home/libelektra/build/lib:${LD_LIBRARY_PATH} make package
