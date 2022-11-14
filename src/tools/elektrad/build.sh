#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@ || exit

cd elektrad || exit
go install .
cd ..
