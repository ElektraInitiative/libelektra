FROM ubuntu:jammy

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        bison \
        build-essential \
        clang \
        cmake \
        curl \
        debhelper \
        debsigs \
        devscripts \
        dh-exec \
        dh-lua \
        dh-python \
        discount \
        doxygen \
        flex \
        ghostscript \
        git \
        git-buildpackage \
        graphviz \
        libaugeas-dev \
        libcurl4-gnutls-dev \
        libdbus-1-dev \
        libev-dev \
        libgit2-dev \
        libglib2.0-dev \
        libgpgme-dev \
        liblua5.4-dev \
        libmarkdown2-dev \
        libpcre3 \
        libpcre3-dev \
        libqt5svg5-dev \
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libxml2-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        lintian \
        locales \
        ninja-build \
        openjdk-17-jdk \
        pkg-config \
        python3-all \
        python3-dev \
        qtbase5-dev \
        qtdeclarative5-dev \
        ruby-dev \
        rustc \
        swig4.0 \
        unzip \
        valgrind \
        python3-pip \
    	golang \
    && rm -rf /var/lib/apt/lists/*

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# see https://github.com/docker-library/golang/issues/225
ENV XDG_CACHE_HOME=/tmp/.cache

# use lua5.4 since dh-lua installs liblua5.4-dev which sets the
# dependency on the generated package to lua5.4
RUN update-alternatives --set lua-interpreter /usr/bin/lua5.4

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.12.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Update cache for shared libraries
RUN ldconfig

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
