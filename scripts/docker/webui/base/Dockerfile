# base image for elektra web, all other images build upon this (builds elektra with `yajl` and `kdb`)

FROM ubuntu:20.04

# elektra deps
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-get update -y && apt-get install -y cmake git build-essential libyajl-dev curl npm

ENV GO111MODULE=on
ENV LD_LIBRARY_PATH=/usr/local/lib

# elektra web deps
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y nodejs golang-go

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.12.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# add elektra
RUN mkdir -p /home/elektra
WORKDIR /home/elektra/libelektra
ADD . /home/elektra/libelektra/

# build & install libelektra
RUN mkdir -p /home/elektra/libelektra/build
WORKDIR /home/elektra/libelektra/build
RUN cmake .. -DTOOLS="kdb;web" && make -j ${PARALLEL} && make install

# prepare user and home dir
RUN groupadd -r elektra && useradd --no-log-init -r -g elektra elektra && chown -R elektra:elektra /home/elektra
