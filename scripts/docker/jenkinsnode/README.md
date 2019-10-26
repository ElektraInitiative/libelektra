# Configuring Elektra as a Jenkins Build Node with Docker

This Dockerfile builds a dockerimage based on debian:stretch including all
requirements to build Elektra and most of its bindings/plugins to act as a build
node for Jenkins. It contains an ssh server and a java environment to allow
jenkins to connect to the container and execute commands on top of that.

Before building the image, you may want to adjust the password for jenkins to
something more secure, so adjust the line `echo "jenkins:<password>" | chpasswd && \`
in the Dockerfile.

Optionally you can modify the file `run_make` if you want to
increase the number of jobs for the make command on the build container. You can
also modify `run_ionice` to adjust the build process' priority, which is set to
idle by default in order to keep the host system responsive. In case you want to
disable that, change the line `ionice -c 3 nice $*` in `run_ionice` to simply `$*`.

Now build the image with the following command:

```sh
docker build -t buildelektra-stretch .
```

After the build process has completed you can create and run a Docker container
that uses the image we just created. Additionally you need to forward the ssh
port to some port you want to use on the container's host so jenkins can connect
to it. We give this container a name to easily refer to it at runtime.

```sh
docker run -d -p 22222:22 --name build-v2 buildelektra-stretch
```

Now you should be able to connect to the container locally via the host machine:

```sh
ssh jenkins@localhost -p 22222
```

Afterwards you need to ensure that this ssh port is also accessible from the
outside, so that the jenkins master server can connect to the build node.
As this greatly depends on the given network setup we can't give an exact
explanation for it, but you may need to expose the local port to the outside or
even use SSH tunneling or forwarding in case your build node doesn't have direct
access to the jenkins master server.

Once this works, you have to add the credentials for the container's jenkins
user to the main jenkins server and connect to it. All done!
