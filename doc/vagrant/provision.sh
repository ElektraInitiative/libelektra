#!/bin/sh

SWIGVERSION="3.0.11"

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
    checkinstall \
&& rm -rf /var/lib/apt/lists/* \
&& apt-get clean

# install swig 3
cd /tmp
curl -fsS "https://codeload.github.com/swig/swig/tar.gz/rel-${SWIGVERSION}" | tar xz
cd "swig-rel-${SWIGVERSION}" && ./autogen.sh && ./configure && make && make install
cd && rm -rf "/tmp/swig-rel-${SWIGVERSION}"
