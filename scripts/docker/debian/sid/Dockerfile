FROM debian:sid

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        build-essential \
        checkinstall \
        clang-6.0 \
        clang-format-6.0 \
        cmake \
        curl \
        dh-lua \
        gawk \
        git \
        gobject-introspection \
        icheck \
        lcov \
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
        libzmq3-dev \
        llvm \
        maven \
        moreutils \
        npm \
        openjdk-8-jdk \
        pkg-config \
        python-dev \
        python-pip \
        python3-dev \
        ruby-dev \
        sloccount \
        swig3.0 \
        systemd \
        tclcl-dev \
        valgrind \
    && rm -rf /var/lib/apt/lists/*

RUN pip install cmake-format==0.4.5 pyyaml

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Handle Java
RUN echo 'export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")'>> /etc/bash.bashrc
RUN echo '\
/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/\n\
/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/\n' > /etc/ld.so.conf.d/jdk.conf
RUN ldconfig

# Prettier
RUN npm install --global prettier@1.16.4

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
