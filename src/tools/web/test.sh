#!/bin/sh

echo " -~- Testing API elektra-web > elektrad -~- "
cd elektrad
go test ./...
cd ..

echo " -~- Testing API elektra-web > webd -~- "
cd webd
npm install
npm test
cd ..
