#!/bin/sh
cd @CMAKE_CURRENT_SOURCE_DIR@ || exit

echo " -~- Testing API elektra-web > elektrad -~- "
cd elektrad || exit
go test ./...
cd ..
