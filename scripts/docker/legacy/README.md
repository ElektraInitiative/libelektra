# Trying out Elektra with Docker

This Dockerfile builds a dockerimage based on ubuntu:artful including all requirements to build Elektra.

If [Docker](https://www.docker.com/) is available on your machine change to the directory containing the file you are currently reading and build the image with

```sh
$ docker build -t buildelektra .
```

After the build process has completed you can create and run a Docker container that uses the image we just created.
This runs bash in a container based on the buildelektra image and mounts the `./data` folder into the container.

```sh
$ mkdir data
$ docker run --rm -v ${PWD}/data:/mnt/share -it buildelektra bash
```

The container contains a script that pulls a specified snapshot of Elektra from GitHub and either installs it in the container or builds a .deb package from it.

Try it out from within the container with

```sh
# change into the mounted folder
$ cd /mnt/share
# this builds the commit with the tag "0.8.19"
$ buildelektra "0.8.19"
```

After this command has finished you can safely test your specified Elektrasnapshot in the container.

If you would like to build a .deb package run buildelektra like with the `-b` flag:

```sh
$ buildelektra -b elektra master
```

When you exit the container you will find the created .deb package and the downloaded snapshots of Elektra in the data directory.
