#!/bin/sh

if [ -z "$KDB" ]; then
	KDB=kdb
fi

"$KDB" mount "${CMAKE_INSTALL_PREFIX}"/"${install_directory}"/application-config.json system:"${config_root}""${config_default_profile}" yajl
