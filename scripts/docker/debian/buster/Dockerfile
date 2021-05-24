FROM debian:buster

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        antlr4 \
        automake \
        autotools-dev \
        bison \
        build-essential \
        clang-6.0 \
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
        libboost-dev \
        libc6-dbg \
        libcurl4-gnutls-dev \
        libdbus-1-dev \
        libev-dev \
        libgirepository1.0-dev \
        libgit2-dev \
        libglib2.0-dev \
        libgpgme-dev \
        liblua5.3-dev \
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
        python-all \
        python-pip \
        python3-all \
        qtbase5-dev \
        qtdeclarative5-dev \
        reprepro \
        ruby \
        ruby-dev \
        rustc \
        swig3.0 \
        systemd \
        tclcl-dev \
        uuid-dev \
        unzip \
        valgrind \
        virtualenv \
        wine \
    && rm -rf /var/lib/apt/lists/*

# use lua5.3 since dh-lua installs liblua5.3-dev which sets the
# dependency on the generated package to lua5.3
RUN update-alternatives --set lua-interpreter /usr/bin/lua5.3

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.10.0
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
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-7.0-bin.zip && unzip gradle-7.0-bin.zip && rm gradle-7.0-bin.zip && mv gradle-7.0 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"

USER ${JENKINS_USERID}

# extend PATH for user jenkins to include gradle
RUN echo "export PATH=${PATH}:/opt/gradle/bin" >> /home/jenkins/.bashrc

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'

# shfmt
ENV SHFMT_PATH=/home/jenkins/bin
ENV SHFMT_VERSION=v2.6.3
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
    && cd "${SHFMT_PATH}" \
    && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
    && chmod a+x shfmt

# cmake-format
RUN pip install cmake-format[yaml]==0.6.3

# Coveralls
ENV COVERALLS_VIRTUALENV_PATH=/home/jenkins/coveralls
RUN virtualenv "${COVERALLS_VIRTUALENV_PATH}" \
    && cd "${COVERALLS_VIRTUALENV_PATH}" \
    && . bin/activate \
    && pip install "urllib3==1.22" \
    && pip install pyyaml \
    && pip install cpp-coveralls \
    && deactivate
ENV PATH="${PATH}:${COVERALLS_VIRTUALENV_PATH}/bin"
