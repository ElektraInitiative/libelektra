FROM ubuntu:bionic

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        bison \
        build-essential \
        clang \
        curl \
        debhelper \
        debsigs \
        devscripts \
        dh-exec \
        dh-lua \
        discount \
        doxygen \
        flex \
        git \
        git-buildpackage \
        graphviz \
        libaugeas-dev \
        libdbus-1-dev \
        libgit2-dev \
        libglib2.0-dev \
        libgpgme-dev \
        liblua5.3-dev \
        libmarkdown2-dev \
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libxml2-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        locales \
        ninja-build \
        openjdk-11-jdk \
        pkg-config \
        python3-all \
        python3-dev \
        qtbase5-dev \
        qtdeclarative5-dev \
        ruby-dev \
        swig \
        unzip \
        valgrind \
        python3-pip \
    	golang \
    && rm -rf /var/lib/apt/lists/*

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# see https://github.com/docker-library/golang/issues/225
ENV XDG_CACHE_HOME=/tmp/.cache

# see https://github.com/golang/go/wiki/InvalidFlag
ENV CGO_CFLAGS_ALLOW="directory.*"

# use lua5.3 because dh-lua installs liblua5.3-dev which sets the
# dependency on the generated package to lua5.3
# lua 5.3 doesn't setup lua-interpreter alternative, therefore needs to be
# explicitly set (https://bugs.launchpad.net/ubuntu/+source/lua5.3/+bug/1707212)
RUN update-alternatives --install /usr/bin/lua lua-interpreter /usr/bin/lua5.3 130 \
    && update-alternatives --install /usr/bin/luac lua-compiler /usr/bin/luac5.3 130

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.12.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Install latest CMake version, since current cmake in repository (3.10.2) doesn't
# support packaging of debuginfo
RUN wget https://github.com/Kitware/CMake/releases/download/v3.19.2/cmake-3.19.2.tar.gz \
    && tar -zxvf cmake-3.19.2.tar.gz \
    && rm cmake-3.19.2.tar.gz \
    && cd cmake-3.19.2 \
    && ./bootstrap \
    && make \
    && make install

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers

ARG JENKINS_GROUPID
RUN groupadd \
    -g ${JENKINS_GROUPID} \
    -f \
    jenkins

ARG JENKINS_USERID
RUN useradd \
    --create-home \
    --uid ${JENKINS_USERID} \
    --gid ${JENKINS_GROUPID} \
    --shell "/bin/bash" \
    jenkins

# download and install gradle
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-8.0.2-bin.zip && unzip gradle-8.0.2-bin.zip && rm gradle-8.0.2-bin.zip && mv gradle-8.0.2 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"

USER ${JENKINS_USERID}

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
