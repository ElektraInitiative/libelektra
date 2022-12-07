#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@ || exit

cd ../webui || exit
npm install
npm run build

cd ../webd || exit
