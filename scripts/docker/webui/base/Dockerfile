# base image for elektra web, all other images build upon this (builds elektra with `yajl` and `kdb`)

FROM ubuntu:16.04

# elektra deps
RUN apt-get update -y && apt-get install -y cmake git build-essential libyajl-dev curl nodejs-legacy npm

# elektra web deps
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
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
RUN mkdir /home/elektra/libelektra/build
WORKDIR /home/elektra/libelektra/build
RUN cmake .. -DTOOLS="kdb;web" && make -j ${PARALLEL} && make install

# prepare user and home dir
RUN groupadd -r elektra && useradd --no-log-init -r -g elektra elektra && chown -R elektra:elektra /home/elektra
