#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd ../webui
npm install
npm run build

cd ../webd
