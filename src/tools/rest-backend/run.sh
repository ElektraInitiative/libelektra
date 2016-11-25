#!/bin/sh
@CMAKE_INSTALL_PREFIX@/@install_directory@/@tool@ -c @CMAKE_INSTALL_PREFIX@/@install_directory@/@tool@-config.json &
echo $! > /run/elektra-@tool@.pid
