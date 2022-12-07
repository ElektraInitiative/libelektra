# Introduction

Running all the tests like the build server requires multiple dependencies. To overcome this problem, instead of trying to install all the necessary dependencies on your own, an appropriate Docker image can be used. This way you can easily and quickly run all the tests.

## Who Is This Guide For?

For anyone who wants to run all the tests, like it is done by the build server.

This is a step-by-step guide. Just follow the steps and you are good to go!

## Prerequisites

- Docker for Linux containers has to be pre-installed. Please refer to https://docs.docker.com/install/ if you haven't installed it yet. Your host OS can be either Linux, macOS or Windows. 
- Basic knowledge of Docker (not mandatory)

## Podman support

Alternatively, you can use podman, a different container engine which is compatible with Docker. See https://podman.io/ for more details and an installation guide. If you are using podman, and want to follow this tutorial, just replace the docker command with podman.


## What to Begin With?

### 1. Docker Image

To build your own Docker image, run the following command from the source root directory:

```sh
docker build -t buildelektra-bullseye \
--build-arg JENKINS_USERID=$(id -u) \
--build-arg JENKINS_GROUPID=$(id -g) \
-f scripts/docker/debian/bullseye/Dockerfile \
scripts/docker/debian/bullseye/
# RET: 0
```

The build process depends on your Internet connection speed and the overall performance of your hardware. Most likely, it will take at least
5 minutes. Please be patient. Once you have built the image, you can reuse it multiple times.

The image tag `buildelektra-bullseye` we suggested can be replaced by a name of your own choosing.

Another alternative but not recommended(!) option would be to pick one of the publicly available Docker images of Elektra. If you do not know the difference, just pick this one --> "build-elektra-debian-stretch".
Unfortunately, it will take some time to download it, since it is pretty big, but you can be sure you'll have all the needed dependencies.
You can choose a light-weight Alpine image which won't take long to download, however it is not recommended. This image does not contain all necessary dependencies.

If you want to view all the available images, execute this command:

```shell
docker run --rm anoxis/registry-cli -r https://hub-public.libelektra.org
```

You will see something like this:

```
---------------------------------
Image: build-elektra-debian-stretch
  tag: 201906-ecf9161f41a8b472b3b0282a85a9f91d1f0f45357756e5451ae043fce8d0100e
  tag: 201902-b6d49f470e1171348248b2f87ef397d58d7a2dae14d201f4073564079ce0c070
  tag: 201903-b95dc56352aa684e16dfb8628bded4c69c712223f5d7ed99ebdd644852a32123
  tag: 201905-ecf9161f41a8b472b3b0282a85a9f91d1f0f45357756e5451ae043fce8d0100e
  tag: 201901-6b08855f13ba26e3ad1fa80e399b87df860cc24889f2d1854fa0050834567b26
  tag: 201904-ecf9161f41a8b472b3b0282a85a9f91d1f0f45357756e5451ae043fce8d0100e
  tag: 201904-1a6be7b9c3740a2338b14d08c757332cae5254ce58219b6cc2908c7bd6e4f460
  tag: 201903-1a6be7b9c3740a2338b14d08c757332cae5254ce58219b6cc2908c7bd6e4f460
  tag: 201903-6b08855f13ba26e3ad1fa80e399b87df860cc24889f2d1854fa0050834567b26
  tag: 201902-6b08855f13ba26e3ad1fa80e399b87df860cc24889f2d1854fa0050834567b26
```

Afterwards pull your desired image as you would do from any public registry:

```shell
docker pull hub-public.libelektra.org/<image_name>:<tag_name>
```

Example:

```shell
docker pull hub-public.libelektra.org/build-elektra-debian-stretch:201905-9dfe329fec01a6e40972ec4cc71874210f69933ab5f9e750a1c586fa011768ab
```

### 2. Run the Docker Container

You have to be in the root of the source directory of the Elektra Initiative, so that the container can properly map all the source files.

So from your root source folder run the following:

```sh
docker run -it --rm \
-v "$PWD:/home/jenkins/workspace" \
-w /home/jenkins/workspace \
buildelektra-bullseye
```

#### 2.a Run the Docker Container with podman

It is recommended that you run your container as a non-root user. As a result, you have to set the correct permissions of the source directory.

With `podman unshare ls -al` you can see the currently set owner and permissions from the container's perspective.

The output inside the root source directory might look something like this:

```
drwxr-xr-x. 1 root root   424  3. Nov 13:08 .
drwxr-xr-x. 1 root root    20  3. Nov 13:08 ..
drwxr-xr-x. 1 root root   238  3. Nov 13:08 benchmarks
-rw-r--r--. 1 root root 10522  3. Nov 13:08 .cirrus.yml
-rw-r--r--. 1 root root  1479  3. Nov 13:08 .clang-format
-rw-r--r--. 1 root root  1621  3. Nov 13:08 .cmake-format.yaml
-rw-r--r--. 1 root root  3991  3. Nov 13:08 CMakeLists.txt
-rw-r--r--. 1 root root  3171  3. Nov 13:08 CODE_OF_CONDUCT.md
-rw-r--r--. 1 root root    25  3. Nov 13:08 .coveralls.yml
drwxr-xr-x. 1 root root   692  3. Nov 13:08 doc
drwxr-xr-x. 1 root root   888  3. Nov 13:08 examples
drwxr-xr-x. 1 root root   138  3. Nov 13:08 .git
drwxr-xr-x. 1 root root   148  3. Nov 13:08 .github
-rw-r--r--. 1 root root  1422  3. Nov 13:08 .gitignore
drwxr-xr-x. 1 root root   132  3. Nov 13:08 .idea
-rw-r--r--. 1 root root   558  3. Nov 13:08 lgtm.yml
-rw-r--r--. 1 root root  1530  3. Nov 13:08 LICENSE.md
drwxr-xr-x. 1 root root   134  3. Nov 13:08 LICENSES
-rw-r--r--. 1 root root   142  3. Nov 13:08 .oclint
-rw-r--r--. 1 root root  9714  3. Nov 13:08 README.md
-rw-r--r--. 1 root root   643  3. Nov 13:08 .restyled.yaml
drwxr-xr-x. 1 root root     8  3. Nov 13:08 .reuse
drwxr-xr-x. 1 root root   956  3. Nov 13:08 scripts
drwxr-xr-x. 1 root root   110  3. Nov 13:08 src
drwxr-xr-x. 1 root root   316  3. Nov 13:08 tests
```

Inside the source directory, you can change the permissions to any user id with `podman unshare chown 1000:1000 -R .`. Keep in mind that this
changes the _host_ filesystem. You can read more about this [here](https://docs.podman.io/en/latest/markdown/podman-run.1.html).

Finally, you can run the container with:

```shell
podman run --user 1000 -it --rm \
-v "$PWD:/home/jenkins/workspace:Z" \
-w /home/jenkins/workspace buildelektra-bullseye
```

Do not forget the `:Z` label. You can read more about the labels in the podman [documentation](https://docs.podman.io/en/latest/markdown/podman-run.1.html#volume-v-source-volume-host-dir-containe).

### 3. Build

After starting the container, you should be automatically inside it in the working directory `/home/jenkins/workspace`.

Create a folder where Elektra will be installed, create another folder for building the source and `cd` to it and like this:

```shell
mkdir elektra-install && mkdir elektra-build-docker && cd elektra-build-docker
```

Build it with

```sh
cmake /home/jenkins/workspace \
-DBINDINGS="ALL;-DEPRECATED" \
-DPLUGINS="ALL;-DEPRECATED" \
-DTOOLS="ALL" \
-DENABLE_DEBUG="ON" \
-DKDB_DB_HOME="/home/jenkins/workspace/elektra-build-docker/.config/kdb/home" \
-DKDB_DB_SYSTEM="/home/jenkins/workspace/elektra-build-docker/.config/kdb/system" \
-DKDB_DB_SPEC="/home/jenkins/workspace/elektra-build-docker/.config/kdb/spec" \
-DBUILD_DOCUMENTATION="OFF" \
-DCMAKE_RULE_MESSAGES="OFF" \
-DCMAKE_INSTALL_PREFIX="/home/jenkins/workspace/elektra-install" \
-DCOMMON_FLAGS="-Werror"
```

and then with

```shell
make -j 10
```

The number 10 can be changed as follows: number of supported simultaneous threads by your CPU + 2. But don't worry, this can only affect the speed of the building, it cannot really break it.

Additionally, you may want to install Elektra inside the container, because the installed tests rely on it.
The build server also does this and it is possible that the installed tests have different results (e.g. if test data is missing)
Just run this command:

```sh
make install
```

After Elektra has been installed we need to add it to the PATH variable, meaning you and the tests can interact with Elektra by typing/executing `kdb` in the command line.

```sh
export PATH="/home/jenkins/workspace/elektra-install/bin:$PATH"
export LD_LIBRARY_PATH="/home/jenkins/workspace/elektra-install/lib:$LD_LIBRARY_PATH"
export LUA_CPATH="/home/jenkins/workspace/elektra-install/lib/lua/5.2/?.so;"
```

### 4. Run Tests

Finally, run the tests. There are two sets of tests. Run the first one with this command:

```sh
make run_all
```

For the second set to run, remember to execute `make install` from the previous step. Run the second set with this command:

```sh
kdb run_all
```
