# run this container with any of the run scripts (in order to mount the source code folder, ensure privileged mode to be able to mount fuse, setup logging to nohup.out/stdout)

FROM ubuntu:focal 

RUN apt-get -y update
RUN apt-get -y update

#install everything for fuse
RUN apt-get -y install fuse python3-pip
RUN pip3 install fusepy psutil

#install convinience packages for interactive use
RUN apt-get -y install vim less tree xattr jp2a wget sudo

#setting timezone so that configuring tzdata can happen non-interactively
ENV TZ=Europe/Vienna
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/time

#install elektra
#add elektra sources and 'apt update'
RUN echo "deb [trusted=yes] https://debs.libelektra.org/focal focal-unstable main" | tee /etc/apt/sources.list.d/elektra.list
RUN apt-get update
RUN apt-get -y install libelektra5-all

WORKDIR /root

#create mountpoints
RUN mkdir /root/mount
RUN mkdir /root/mount_gopts_example

#copy source, build & install elektra_fuse wheel
COPY ./ /root/elektra_fuse/

#LICENSE.md will not be included inside the package built inside the container (to do that, the build context of docker would need to include the project root)
RUN cd /root/elektra_fuse && make build && make install
