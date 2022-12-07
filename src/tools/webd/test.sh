#!/bin/sh
cd @CMAKE_CURRENT_SOURCE_DIR@ || exit

echo " -~- Testing API elektra-web > webd -~- "
cd webd || exit
npm install
npm test
cd ..
