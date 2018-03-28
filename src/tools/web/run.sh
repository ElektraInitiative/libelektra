#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@

cd client
npm run start:prod
