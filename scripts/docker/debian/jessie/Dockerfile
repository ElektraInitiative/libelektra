FROM debian:jessie

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
    curl \
    build-essential \
    clang \
    autotools-dev \
    automake \
    cmake \
    pkg-config \
    bison \
    ruby \
    ruby-dev \
    python-dev \
    python3-dev \
    libpython3-dev \
    dh-lua \
    liblua5.2-dev \
    tclcl-dev \
    libaugeas-dev \
    libbotan1.10-dev \
    libyajl-dev \
    git \
    libgit2-dev \
    libboost-all-dev \
    libssl-dev \
    libdbus-1-dev \
    libpcre3-dev \
    libpcre++-dev \
    libglib2.0-dev \
    libxerces-c-dev \
    libyaml-cpp-dev \
    swig3.0 \
    libuv0.10-dev \
    ghc \
    cabal-install \
    c2hs \
    checkinstall \
    gobject-introspection \
    libgirepository1.0-dev \
    systemd \
    libsystemd-dev \
    gawk \
    lcov \
    valgrind && \
    rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install hspec QuickCheck


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
