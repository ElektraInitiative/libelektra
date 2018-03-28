#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd elektrad
npm install
npm run build
cd ..

cd client
npm install
npm run build
cd ..
