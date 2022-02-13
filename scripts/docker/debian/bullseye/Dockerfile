FROM debian:bullseye

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        antlr4 \
        automake \
        autotools-dev \
        bison \
        build-essential \
        clang-11 \
        cmake \
        curl \
        debsigs \
        default-jdk \
        devscripts \
        dh-exec \
        dh-lua \
        diffutils \
        discount \
        dnsutils \
        doxygen \
        ed \
        flex \
        gawk \
        ghostscript \
        git \
        git-buildpackage \
        gobject-introspection \
        graphviz \
        icheck \
        lcov \
        libantlr4-runtime-dev \
        libaugeas-dev \
        libc6-dbg \
        libcurl4-gnutls-dev \
        libdbus-1-dev \
        libev-dev \
        libgirepository1.0-dev \
        libgit2-dev \
        libglib2.0-dev \
        libgpgme-dev \
        liblua5.4-dev \
        libmarkdown2-dev \
        libpcre++-dev \
        libpcre3-dev \
        libpython3-dev \
        libqt5svg5-dev \
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        lintian \
        llvm \
        mingw-w64 \
        ninja-build \
        patch \
        patchutils \
        pkg-config \
        python3-all \
        python3-pip \
        python3-all \
        qtbase5-dev \
        qtdeclarative5-dev \
        reprepro \
        ruby \
        ruby-dev \
        rustc \
        swig4.0 \
        systemd \
        tclcl-dev \
        uuid-dev \
        unzip \
        valgrind \
        virtualenv \
        wine \
    && rm -rf /var/lib/apt/lists/*

# Build dependency for libelektra-fuse
RUN pip3 install wheel reuse

# use lua5.4 since dh-lua installs liblua5.4-dev which sets the
# dependency on the generated package to lua5.4
RUN update-alternatives --set lua-interpreter /usr/bin/lua5.4

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.11.0
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
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-7.4-bin.zip && unzip gradle-7.4-bin.zip && rm gradle-7.4-bin.zip && mv gradle-7.4 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"

USER ${JENKINS_USERID}



# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'

# shfmt
ENV SHFMT_PATH=/home/jenkins/bin
ENV SHFMT_VERSION=v3.3.1
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
    && cd "${SHFMT_PATH}" \
    && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
    && chmod a+x shfmt

# cmake-format
RUN pip3 install cmake-format[yaml]==0.6.13

# Coveralls
ENV COVERALLS_VIRTUALENV_PATH=/home/jenkins/coveralls
RUN virtualenv "${COVERALLS_VIRTUALENV_PATH}" \
    && cd "${COVERALLS_VIRTUALENV_PATH}" \
    && . bin/activate \
    && pip3 install "urllib3==1.26" \
    && pip3 install "pyyaml==5.4.1" \
    && pip3 install cpp-coveralls \
    && deactivate
ENV PATH="${PATH}:${COVERALLS_VIRTUALENV_PATH}/bin"
