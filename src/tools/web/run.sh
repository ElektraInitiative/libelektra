#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd elektrad
npm start &
echo $! > /run/elektra-@tool@-elektrad.pid
cd ..

cd client
npm start &
echo $! > /run/elektra-@tool@.pid
