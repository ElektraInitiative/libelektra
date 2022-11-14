#!/usr/bin/env bash

#make sure we have the right build context
cd $(dirname "$0")/.. || exit

docker build -f ./docker/Dockerfile --tag elektra-deb:1.0 .
