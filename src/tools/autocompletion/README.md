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

## Fish

### Installation

```
# install the fish shell
apt install fish
# run the fish shell
fish
kdb mount $(pwd)/spec/kdb-spec system:/spec/autocomplete/kdb -f ni
# create a symbolic link for the shell completion
ln -s src/tools/autocompletion/kdb.fish ~/.config/fish/completions/kdb.fish
```

Now you are able to have a basic command completion. The most important commands and the completion of keys are supported.
If you change something in the _kdb.fish_ file, the shell will auto-refresh.
