FROM ubuntu:bionic

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
        locales \
        curl \
        debhelper \
        dh-exec \
        libzmq3-dev \
        qtbase5-dev \
        qtdeclarative5-dev \ 
        libmarkdown2-dev \ 
        discount \
        dh-lua \
        python-all \ 
        python3-all \
        python-dev \
        python3-dev \
        libgtest-dev \
        libsystemd-dev \ 
        liblua5.2-dev \
        libbotan-2-dev \ 
        doxygen \
        graphviz \
        build-essential \
        autotools-dev \
        automake \
        cmake \
        devscripts \
        pkg-config \
        ruby-dev \
        libxml2-dev \
        libaugeas-dev \
        libyajl-dev \
        git \
        libxerces-c-dev \
        libgpgme-dev \
        libgit2-dev \
        libboost-dev \
        libssl-dev \
        libdbus-1-dev \
        swig \
    && rm -rf /var/lib/apt/lists/*

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers

ARG JENKINS_GROUPID
RUN groupadd \
    -g ${JENKINS_GROUPID} \
    jenkins

ARG JENKINS_USERID
RUN useradd \
    --create-home \
    --uid ${JENKINS_USERID} \
    --gid ${JENKINS_GROUPID} \
    --shell "/bin/bash" \
    jenkins

USER ${JENKINS_USERID}