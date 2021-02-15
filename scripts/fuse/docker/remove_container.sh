#!/bin/bash

docker rm $(docker stop $(docker ps -a -q --filter ancestor="elektra-deb:1.0" --format="{{.ID}}"))
