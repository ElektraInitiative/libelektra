#!/usr/bin/env bash

#create test user
useradd -m testuser

#test casdacing with different users
sudo -u testuser kdb set user:/person/name "Alexander Firbas"
sudo -u testuser kdb set user:/person/alter 22
sudo -u testuser kdb set user:/info "This is information originating from user:/info (testuser)"
sudo -u testuser kdb set user:/person "This key has metadata"
sudo -u testuser kdb meta-set user:/person height 180

sudo -u root kdb set user:/person/name "root"
sudo -u root kdb set user:/info "This is information originating from user:/info (root)"

# ... and with a fallback system: key
kdb set system:/person/name "This is a system:/ fallback"

#test dirs that are files at the same time
kdb set user:/dir_and_file_at_once "Contents of a non-leaf node"
kdb set user:/dir_and_file_at_once/leaf "leaf node"

#test the dir namespace

#need to be created from the leafes to the top
mkdir -p /root/dirkeys/{a,b}

cd /root/dirkeys/a
kdb set dir:/dirkey "Key from /root/dirkeys/a"

cd /root/dirkeys/b
kdb set dir:/dirkey "Key from /root/dirkeys/b"

cd /root/dirkeys
kdb set dir:/dirkey "Key from /root/dirkeys"

#create dummy processes (that expose different values in dir:)
cd /root/dirkeys/a
sudo -u testuser -i nohup sleep infinity &
cd /root/dirkeys/b
sudo -u root -i nohup sleep infinity &

#gopts example (adapted from test_gopts.py)
cd /root/elektra_fuse/docker/
./create_gopts_keys.py
gcc -o do_nothing do_nothing.c
nohup ./do_nothing get -v user:/ &
echo $! > /root/pid_of_process_using_gopts
#create key that is masked by a proc: key in this example
kdb set user:/tests/python/gopts/getter/keyname "I am some key originating from user: and do not come from a command line argument."
