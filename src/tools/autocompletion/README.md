# Autocompletion
To get autocompletion to run you need to have python3 installed and the kdb-python Module up and running.
For the kdb-python Module to run, the `~/.bashrc` needs to contain `export PYTHONPATH=${PYTHONPATH}:/PATH/TO/PYTHON`.

## Setup environment with docker
```
docker run -it debian:buster
apt-get update
apt-get install ca-certificates
apt-get install vim gnupg
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F26BBE02F3C315A19BF1F791A9A25CC1CC83E839
vim /etc/apt/sources.list
```
add the line deb https://debs.libelektra.org/buster buster main
```
apt-get update
apt-get install elektra-bin
apt-get install python3-elektra
```

## Bash
### Installation
```
source kdb_bash.sh
kdb mount $(pwd)/spec/kdb-spec system:/spec/autocomplete/kdb -f ni
```
