FROM ubuntu:bionic

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        build-essential \
        clang \
        cmake \
        curl \
        debhelper \
        devscripts \
        dh-exec \
        dh-lua \
        discount \
        doxygen \
        git \
        git-buildpackage \
        graphviz \
        libaugeas-dev \
        libboost-dev \
        libbotan-2-dev \
        libdbus-1-dev \
        libgit2-dev \
        libgpgme-dev \
        libgtest-dev \
        liblua5.2-dev \
        libmarkdown2-dev \
        libssl-dev \
        libsystemd-dev \
        libxerces-c-dev \
        libxml2-dev \
        libyajl-dev \
        libzmq3-dev \
        locales \
        pkg-config \
        python-all \
        python-dev \
        python3-all \
        python3-dev \
        ninja-build \
        qtbase5-dev \
        qtdeclarative5-dev \
        ruby-dev \
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
    -f \
    jenkins

ARG JENKINS_USERID
RUN useradd \
    --create-home \
    --uid ${JENKINS_USERID} \
    --gid ${JENKINS_GROUPID} \
    --shell "/bin/bash" \
    jenkins

USER ${JENKINS_USERID}

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
