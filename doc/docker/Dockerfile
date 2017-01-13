FROM ubuntu:xenial

RUN apt-get -qq update && apt-get -qq -y install \
    curl \
    build-essential \
    autotools-dev \
    automake \
    cmake \
    pkg-config \
    doxygen \
    graphviz \
    bison \
    ruby-dev \
    python-dev \
    python3-dev \
    libpython3-dev \
    liblua5.3-dev \
    tclcl-dev \
    libaugeas-dev \
    libyajl-dev \
    libgit2-dev \
    libboost-all-dev \
    libssl-dev \
    libdbus-1-dev \
    libpcre3-dev \
    libpcre++-dev \
    checkinstall
    && gem install ronn

# install Swig 3
ADD https://codeload.github.com/swig/swig/tar.gz/rel-3.0.11 /root/swig.tar.gz
RUN cd && tar xzf swig.tar.gz && cd swig-rel-3.0.11 && ./autogen.sh && ./configure && make && make install

# Housekeeping
RUN apt autoremove -qq -y \
    && rm -rf /var/lib/apt/lists/* && rm -rf /tmp/*
