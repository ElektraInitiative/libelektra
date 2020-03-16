#!/bin/sh
cd @CMAKE_CURRENT_SOURCE_DIR@

echo " -~- Testing API elektra-web > elektrad -~- "
cd elektrad
go test ./...
cd ..
