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
        clang-12 \
        clang-format-12 \
        cmake \
        curl \
        dh-lua \
        ed \
        flex \
        gawk \
        git \
        gnupg2 \
        gobject-introspection \
        icheck \
        lcov \
        libantlr4-runtime-dev \
        libaugeas-dev \
        libc6-dbg \
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
        llvm-12 \
        moreutils \
        ninja-build \
        npm \
        openjdk-11-jdk \
        pkg-config \
        python3-dev \
        python3-pip \
        ruby \
        ruby-dev \
        sloccount \
        swig4.0 \
        systemd \
        tclcl-dev \
        unzip \
        valgrind \
        wget \
    && rm -rf /var/lib/apt/lists/*

# Add Clang 12 to path
ENV PATH="/usr/lib/llvm-12/bin:$PATH"

# Update cache for shared libraries
RUN ldconfig

RUN pip3 install cmake-format[yaml]==0.6.13

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.11.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Prettier
RUN npm install --global prettier@2.2.1

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

# download and install gradle
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-7.1.1-bin.zip && unzip gradle-7.1.1-bin.zip && rm gradle-7.1.1-bin.zip && mv gradle-7.1.1 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"

USER ${JENKINS_USERID}

# extend PATH for user jenkins to include gradle
RUN echo "export PATH=${PATH}:/opt/gradle/bin" >> /home/jenkins/.bashrc

# shfmt
ENV SHFMT_PATH=/home/jenkins/bin
ENV SHFMT_VERSION=v3.2.4
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
    && cd "${SHFMT_PATH}" \
    && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
    && chmod a+x shfmt

# Ronn-NG
ENV PATH="$PATH:/home/jenkins/.local/share/gem/ruby/2.7.0/bin"
RUN gem install --user-install ronn-ng -v 0.10.1.pre1 && ronn --version
