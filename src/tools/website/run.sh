#!/bin/sh
cd @CMAKE_INSTALL_PREFIX@/@install_directory@ && ./node_modules/grunt-cli/bin/grunt server &
echo $! > "$(kdb sget "${config_root}""${config_default_profile}"/daemon/lock /run/elektra-@tool@.pid)"
