# Introduction

Podman is an alternative container engine which does not require root
privileges on the host OS.

Podman aims at being compatible with Docker and in most cases simply replacing
`docker` by `podman` is enough.

However, there are some notable differences when running containers themselves
as a user other than root.

## Who Is This Guide For?

Anyone who uses Podman to run their containers.

## Prerequisites

- A working Podman installation

## Basics

In most cases one can simply replace `docker` by `podman` when running a command.

For instance, to build the Debian bullseye container with Docker the command is

```sh
docker build \
  --tag "build-elektra-debian-bullseye" \
	--build-arg "JENKINS_USERID=$(id -u)" \
	--build-arg "JENKINS_GROUPID=$(id -g)" \
	--file "scripts/docker/debian/bullseye/Dockerfile" \
	scripts/docker/debian/bullseye/
```

Simply replacing `docker` with `podman` works in this case

```sh
podman build \
  --tag "build-elektra-debian-bullseye" \
	--build-arg "JENKINS_USERID=$(id -u)" \
	--build-arg "JENKINS_GROUPID=$(id -g)" \
	--file "scripts/docker/debian/bullseye/Dockerfile" \
	scripts/docker/debian/bullseye/
```

## Running a container as a user other than root

One area where Podman notably differs from Docker is when interacting with the
filesystem of the host.

This is relevant when mounting directories of the host filesystem as volumes.

With Docker the container engine itself runs as a priviledged process and
therefore the permissions in the host OS are not relevant.

With Podman the situation is more involved, as the container engine does not
run with root priviledges.

For instance, the containers specified for building Elektra are configured such
that the user inside the container is a non-root user.

If one wishes to mount the Elektra source directory from the host filesystem as
a volume for the container, extra steps are necessary.

Inside the source directory, you can change the permissions to any user id with
`podman unshare chown $(id -u):$(id -u) -R .`. Keep in mind that this changes
the _host_ filesystem. You can read more about this
[here](https://docs.podman.io/en/latest/markdown/podman-run.1.html).

After having changed the permissions, one can run the container

```sh
podman run \
   --user $(id -u) \
   --interactive \
   --tty \
   --rm \
   --volume "$PWD:/home/jenkins/workspace:Z" \
   --workdir "/home/jenkins/workspace" \
   build-elektra-debian-bullseye
```

Do not forget the `:Z` label. You can read more about the labels in the Podman
[documentation](https://docs.podman.io/en/latest/markdown/podman-run.1.html#volume-v-source-volume-host-dir-containe).

Alternatively, if you prefer to not change the permissions of the host
filesystem, you can run the container as a root user. Note however, that the
environment will differ from that used in continuous integration and the usual
caveats concerning running processes as root apply:

```sh
podman run \
   --user "root" \
   --interactive \
   --tty \
   --rm \
   --volume "$PWD:/home/jenkins/workspace:Z" \
   --workdir "/home/jenkins/workspace" \
   build-elektra-debian-bullseye
```
