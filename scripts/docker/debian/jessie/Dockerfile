FROM debian:jessie

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
    automake \
    autotools-dev \
    build-essential \
    checkinstall \
    cmake \
    curl \
    dh-lua \
    gawk \
    git \
    gobject-introspection \
    lcov \
    libaugeas-dev \
    libboost-all-dev \
    libbotan1.10-dev \
    libdbus-1-dev \
    libev-dev \
    libgirepository1.0-dev \
    libgit2-dev \
    libglib2.0-dev \
    libgpgme11-dev \
    liblua5.2-dev \
    libpcre++-dev \
    libpcre3-dev \
    libpython3-dev \
    libssl-dev \
    libsystemd-dev \
    libuv0.10-dev \
    libxerces-c-dev \
    libyajl-dev \
    libzmq3-dev \
    ninja-build \
    pkg-config \
    python-dev \
    python3-dev \
    ruby \
    ruby-dev \
    swig3.0 \
    systemd \
    tclcl-dev \
    valgrind \
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
