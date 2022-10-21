#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@ || exit

npm run start:prod
