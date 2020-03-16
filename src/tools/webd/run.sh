#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd webd
npm run start:prod
