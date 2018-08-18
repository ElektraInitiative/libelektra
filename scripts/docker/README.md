# Elektra Docker Artifacts

This folder contains all Docker related artifacts.

A list of all Dockerfiles used by the build server can be found in the
[Jenkinsfile](https://master.libelektra.org/scripts/jenkins/Jenkinsfile).

> **Note:**
> Any commands in this file are expected to be run from the root
> of the repository.

## Building Images locally

If you want to run or test Elektra via our Docker images you currently have
to build them yourself.
You can do so by running the following command:

```sh
docker build -t buildelektra-stretch-full \
    --build-arg JENKINS_USERID=`id -u` \
    --build-arg JENKINS_GROUPID=`id -g` \
    -f scripts/docker/debian/stretch/Dockerfile \
    scripts/docker/debian/stretch/
```

You can adapt the targetted Dockerfile via `-f`.
You should also adjust the tag used via `-t` if you are building a different
image.

Please note that the complete images used to test Elektra are quite big
(~3.7GB) and take a some time (15min+) to build.

## Testing Elektra via Docker images

To replicate errors on the test server you can build the image that ran the
test as shown above.

Afterwards you can start the container via the following command:

```sh
docker run -it --rm \
    -v `pwd`:/home/jenkins/workspace \
    -w /home/jenkins/workspace \
    buildelektra-stretch-full
```

Note since we used matching userid + groupid to your current user the container
will write to your mounted directory with the correct permissions.

Once you are running inside the container you can use the same commands as you
would use normally to
[configure/compile](https://master.libelektra.org/doc/COMPILE.md)
and [test](https://master.libelektra.org/doc/TESTING.md) Elektra.
There is also some information on how the
[build server](https://master.libelektra.org/doc/BUILDSERVER.md) uses
the Docker images as well as the actual instructions executed by the
build server in our
[Jenkinsfiles](https://master.libelektra.org/scripts/jenkins).


## Differences to the build server

The build server does not create a bash shell inside the containers.
Instead it runs a nonterminating command (usually `cat`) to keep the container
open.

Afterwards it executes each step via `docker exec`.

There might be some more differences that might influence test outcomes.
One such documented case is [#2008](https://issues.libelektra.org/2008) and
[#1973](https://issues.libelektra.org/1973) where errors on the test
server could not be replicated when running identical commands locally.
