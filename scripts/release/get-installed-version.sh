#!/bin/sh

export LD_LIBRARY_PATH=${WORKSPACE}/system/lib:$LD_LIBRARY_PATH
export PATH=${WORKSPACE}/system/bin:$PATH
export DBUS_SESSION_BUS_ADDRESS=$(dbus-daemon --session --fork --print-address)
export LUA_CPATH="${WORKSPACE}/system/lib/lua/5.2/?.so;"

echo $(kdb get system:/elektra/version/constants/KDB_VERSION)
