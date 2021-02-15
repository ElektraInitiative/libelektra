#!/bin/bash 
./remove_container.sh
./build_container.sh
docker run --privileged -it elektra-deb:1.0 /bin/bash -c "nohup ~/docker/create_keys_and_mount_debug.sh && bash"

