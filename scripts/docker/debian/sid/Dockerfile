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
        clang-6.0 \
        clang-format-6.0 \
        cmake \
        curl \
        default-jdk \
        dh-lua \
        ed \
        gawk \
        git \
        gobject-introspection \
        icheck \
        lcov \
        libantlr4-runtime-dev \
        libaugeas-dev \
        libboost-all-dev \
        libbotan-2-dev \
        libdbus-1-dev \
        libev-dev \
        libgirepository1.0-dev \
        libgit2-dev \
        libglib2.0-dev \
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
        python-dev \
        python-pip \
        python3-dev \
        ruby-dev \
        sloccount \
        swig3.0 \
        systemd \
        tao-pegtl-dev \
        tclcl-dev \
        valgrind \
    && rm -rf /var/lib/apt/lists/*

# YAEP
ARG YAEP_VERSION=550de4cc5600d5f6109c7ebcfbacec51bf80d8d3
RUN cd /tmp \
    && mkdir yaep \
    && curl -o yaep.tar.gz \
       -L https://github.com/vnmakarov/yaep/archive/${YAEP_VERSION}.tar.gz \
    && tar -zxvf yaep.tar.gz --strip-components=1 -C yaep \
    && cd yaep \
    && mkdir build \
    && cd build \
    && env CFLAGS='-fPIC' CXXFLAGS='-fPIC' cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make install \
    && cd ../.. \
    && rm -r yaep yaep.tar.gz

# Update cache for shared libraries
RUN ldconfig

RUN pip install cmake-format[yaml]==0.4.5

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Prettier
RUN npm install --global prettier@1.16.4

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
