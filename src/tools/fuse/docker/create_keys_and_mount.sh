#!/usr/bin/env bash
python3 -c "import elektra_fuse;elektra_fuse.main()" ~/mount/ -f True &
python3 -c "import elektra_fuse;elektra_fuse.main()" ~/mount_gopts_example/ -f True --parent_key /tests/python/gopts &
~/elektra_fuse/docker/create_keys.sh
