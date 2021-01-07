FROM ubuntu:bionic

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        build-essential \
        bison \
        clang \
        curl \
        debhelper \
        debsigs \
        default-jdk \
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
        libboost-dev \
        libdbus-1-dev \
        libgit2-dev \
        libgpgme-dev \
        liblua5.2-dev \
        libmarkdown2-dev \
        libssl-dev \
        libsystemd-dev \
        libxerces-c-dev \
        libxml2-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        locales \
        maven \
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
ARG GTEST_VER=release-1.10.0
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
    && cd cmake-3.19.2 \
    && ./bootstrap \
    && make \
    && make install \
    && cd .. \
    && rm -rf cmake-3.19.2 \
    && rm cmake-3.19.2.tar.gz

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
