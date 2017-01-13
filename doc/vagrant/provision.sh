#!/bin/sh

apt-get -qq update && apt-get -qq -y install \
    curl \
    build-essential \
    autotools-dev \
    automake \
    cmake \
    pkg-config \
    doxygen \
    graphviz \
    bison \
    ruby-ronn \
    ruby-dev \
    python-dev \
    python3.4-dev \
    liblua5.2-dev \
    tclcl-dev \
    libaugeas-dev \
    libyajl-dev \
    libgit2-dev \
    libboost-all-dev \
    libssl-dev \
    libcurl4-gnutls-dev \
    libdbus-1-dev \
    libpcre3-dev \
    libpcre++-dev \
    checkinstall


# install swig 3
cd /tmp
curl -sS https://codeload.github.com/swig/swig/tar.gz/rel-3.0.11 | tar xz
cd swig-rel-3.0.11 && ./autogen.sh && ./configure && make && make install
