#!/usr/bin/env bash

RUNNING_CONTAINERS=$(docker ps -a -q --filter ancestor="elektra-deb:1.0" --format="{{.ID}}")

if [ ! -z "$RUNNING_CONTAINERS" ]; then
	# shellcheck disable=SC2086
	docker rm $(docker stop $RUNNING_CONTAINERS)
fi
