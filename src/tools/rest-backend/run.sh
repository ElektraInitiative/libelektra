#!/bin/sh
@CMAKE_INSTALL_PREFIX@/@install_directory@/@tool@ &
echo $! > /run/elektra-@tool@.pid
