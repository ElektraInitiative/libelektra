#!/bin/sh

export PATH="$PATH/@CMAKE_BINARY_DIR@/bin"

echo " -~- Testing API elektra-web > elektrad -~- "
cd elektrad
go test ./...
cd ..

echo " -~- Testing API elektra-web > webd -~- "
cd webd
npm install
npm test
cd ..
