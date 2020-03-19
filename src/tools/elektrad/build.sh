#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd elektrad
go install .
cd ..
