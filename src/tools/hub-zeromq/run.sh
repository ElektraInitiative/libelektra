#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@ && ./hub-zeromq &
echo $! > /run/elektra-@tool@.pid
