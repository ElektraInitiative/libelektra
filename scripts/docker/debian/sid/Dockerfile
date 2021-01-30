FROM debian:sid

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        antlr4 \
        automake \
        autotools-dev \
        bison \
        build-essential \
        checkinstall \
        clang-11 \
        clang-format-11 \
        cmake \
        curl \
        default-jdk \
        dh-lua \
        ed \
        flex \
        libc6-dbg \
        gawk \
        git \
        gnupg2 \
        gobject-introspection \
        icheck \
        lcov \
        libantlr4-runtime-dev \
        libaugeas-dev \
        libboost-all-dev \
        libdbus-1-dev \
        libev-dev \
        libgcrypt20-dev \
        libgirepository1.0-dev \
        libgit2-dev \
        libglib2.0-dev \
        libgpgme-dev \
        liblua5.3-dev \
        libpcre++-dev \
        libpcre3-dev \
        libpython3-dev \
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        llvm \
        maven \
        moreutils \
        ninja-build \
        npm \
        pkg-config \
        python3-dev \
        python3-pip \
        ronn \
        ruby-dev \
        sloccount \
        swig4.0 \
        systemd \
        tclcl-dev \
        valgrind \
    && rm -rf /var/lib/apt/lists/*

# Update cache for shared libraries
RUN ldconfig

RUN pip3 install cmake-format[yaml]==0.6.3

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.10.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Prettier
RUN npm install --global prettier@1.19.1

# hyperfine
ARG HYPERFINE_VERSION=1.5.0
RUN cd /tmp \
    && curl -o hyperfine.deb \
       -L https://github.com/sharkdp/hyperfine/releases/download/v${HYPERFINE_VERSION}/hyperfine_${HYPERFINE_VERSION}_amd64.deb \
    && dpkg -i hyperfine.deb \
    && rm hyperfine.deb

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

# shfmt
ENV SHFMT_PATH=/home/jenkins/bin
ENV SHFMT_VERSION=v2.6.3
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
    && cd "${SHFMT_PATH}" \
    && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
    && chmod a+x shfmt
