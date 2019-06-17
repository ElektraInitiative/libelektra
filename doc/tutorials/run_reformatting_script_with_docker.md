# Introduction

Running the reformat-all script requires multiple dependencies. To overcome this problem, instead of trying to install all the necessary dependencies on your own,
you can easily build a Docker image and run the script inside a Docker container based on this image.

## Who Is This Guide For?

Do you want to run the reformat-all script easily and without any hassle? You've come to the right place.

This is a step-by-step guide. Just follow the steps and you are good to go!

## Prerequisites

- Docker for Linux containers has to be preinstalled. Please refer to https://docs.docker.com/install/ if you haven't installed it yet.
  Your host OS should better be Linux, because we are going to use your current Linux user ID for building the image.
- Basic knowledge of Docker (not mandatory)

## What to Begin With?

### 1. Build Your Own Docker Image

Now you are going to build your own Docker image based on Debian.

From the project root directory run the following command:

```sh
docker build -t buildelektra-sid \
	--build-arg JENKINS_USERID=$(id -u) \
	--build-arg JENKINS_GROUPID=$(id -g) \
	-f scripts/docker/debian/sid/Dockerfile \
	scripts/docker/debian/sid/
```

The build process depends on your Internet connection speed and the overall performance of your hardware. Most likely, it will take at least
5 minutes. Please be patient. Once you have built the image, you can reuse it multiple times.

The image tag `buildelektra-sid` we suggested can be replaced by a name of your own choosing.

### 2. Run the Docker Container

After you built the image, you can execute a container like this:

```sh
docker run -it --rm \
	-v "$PWD:/home/jenkins/workspace" \
	-w /home/jenkins/workspace \
	buildelektra-sid
```

Again, if you changed the image tag `buildelektra-sid`, please change it above as well.

### 3. Running the Script

After starting the container, you should be automatically inside it in the working directory `/home/jenkins/workspace`.

Now run the script:

```sh
scripts/reformat-all
```

All your files should be reformatted afterwards.
