# Introduction

Running the reformat-all script requires to have multiple dependencies installed. To overcome this problem, instead of trying to install all the necessary dependencies on your own,
you can easily build a Docker image and run the script inside a Docker container based on this image.

## Who is this guide for?

Do you want to run the reformat-all script easily and without any hassle? You've come to the right place.

This is a step-by-step guide. Just follow the steps and you are good to go!

## Prerequisites

- Docker for Linux containers has to be preinstalled. Please refer to https://docs.docker.com/install/ if you haven't installed it yet.
  Your host OS should be better Linux, because we are going to use your current Linux user ID for building the image.
- Basic knowledge of Docker (not mandatory)

## What to begin with?

### 1. Build your own Docker image

Now you are going to build your own Docker image based on Debian.

From the project root directory run the following command:

```sh
docker build -t buildelektra-sid \
	--build-arg JENKINS_USERID=$(id -u) \
	--build-arg JENKINS_GROUPID=$(id -g) \
	-f scripts/docker/debian/sid/Dockerfile \
	scripts/docker/debian/sid/
```

The building process depends on your Internet speed connection and the overall performance of your hardware. Most likely, it will take at least
5 minutes. Please be patient. Once you have it built, you will reuse it.

The image tag `buildelektra-sid` we suggested can be of course replaced to your own preference.

### 2. Run Docker container

Now when you built the image, spin-up a container like this:

```sh
docker run -it --rm \
	-v "$PWD:/home/jenkins/workspace" \
	-w /home/jenkins/workspace \
	buildelektra-sid
```

Again, if you changed the image tag `buildelektra-sid`, please change it above as well.

### 3. Running the script

After starting the container, you should be automatically inside it in the working directory `/home/jenkins/workspace`.

Now run the script:

```sh
scripts/reformat-all
```

All your files should be reformatted afterwards.
