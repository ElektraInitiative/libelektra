# Trying out Elektra with Docker

This Dockerfile builds a dockerimage based on ubuntu:xenial including all requirements to build Elektra.

If [Docker](https://www.docker.com/) is available on your machine change to the directory containing the file you are currently reading and build the image with
```bash
$ docker build -t buildelektra .
```

After the build process has completed you can create and run a Docker container that uses the image we just created.
This runs bash in a container based on the buildelektra image and mounts the `./share` folder into the container.
```bash
$ docker run --rm -v ${PWD}/share:/share -it buildelektra bash
```

In share there is an exemplary shell script that pulls a specified snapshot of Elektra from Github, installs it in the container and builds a .deb package from it.

Try it out from within the container with
```bash
$ cd /share
# this builds the commit with the tag "0.8.19"
$ ./packelektra.sh "0.8.19"
```

After this command has finished you can safely test your specified Elektrasnapshot in the container.
