#!/bin/sh
cd @CMAKE_CURRENT_SOURCE_DIR@

echo " -~- Testing API elektra-web > webd -~- "
cd webd
npm install
npm test
cd ..
