FROM ubuntu:disco

RUN apt-get update && apt-get -y install \
        antlr4 \
        bison \
        build-essential \
        clang-8 \
        cmake \
        curl \
        ed \
        git \
        libantlr4-runtime-dev \
        libyaml-cpp-dev \
        llvm \
        locales \
        moreutils \
        ninja-build \
        perl \
        pkg-config \
        python3-dev \
        ruby-dev \
        tao-pegtl-dev \
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

# flamegraph.pl
ARG FLAME_GRAPH_PATH=/home/jenkins/bin
ENV PATH="${FLAME_GRAPH_PATH}:${PATH}"
RUN mkdir -p "${FLAME_GRAPH_PATH}" \
    && curl -L "https://raw.githubusercontent.com/brendangregg/FlameGraph/master/flamegraph.pl" -o "${FLAME_GRAPH_PATH}/flamegraph" \
    && chmod a+x "${FLAME_GRAPH_PATH}/flamegraph"
