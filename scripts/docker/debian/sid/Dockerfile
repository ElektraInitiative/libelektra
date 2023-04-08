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
        clang-15 \
        clang-format-15 \
        cmake \
        cmake-format \
        curl \
    	dbus-x11 \
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
        libpcrecpp0v5 \
        libpcre3-dev \
        libpython3-dev \
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libxfconf-0-dev \
        libyajl-dev \
        libyaml-cpp-dev \
        libzmq3-dev \
        llvm-15 \
        moreutils \
        ninja-build \
        npm \
        openjdk-17-jdk \
        pkg-config \
        python3-dev \
        python3-wheel \
        ruby \
        ruby-dev \
        shellcheck \
        sloccount \
        swig4.0 \
        systemd \
        tclcl-dev \
        unzip \
        valgrind \
        wget \
    	golang \
    && rm -rf /var/lib/apt/lists/*

# Add Clang 15 to path
ENV PATH="/usr/lib/llvm-15/bin:$PATH"

# Update cache for shared libraries
RUN ldconfig

# see https://github.com/docker-library/golang/issues/225
ENV XDG_CACHE_HOME=/tmp/.cache

# see https://github.com/golang/go/wiki/InvalidFlag
ENV CGO_CFLAGS_ALLOW="directory.*"
ENV LD_LIBRARY_PATH=directory/lib:${LD_LIBRARY_PATH}

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.12.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# Prettier
RUN npm install --global prettier@2.8.4

# Google Java formatter
ARG GOOGLE_JAVA_FORMAT_VER=1.11.0
RUN cd /opt \
    && curl -o google-java-format.jar \
      -L https://github.com/google/google-java-format/releases/download/v${GOOGLE_JAVA_FORMAT_VER}/google-java-format-${GOOGLE_JAVA_FORMAT_VER}-all-deps.jar

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
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-8.0.2-bin.zip && unzip gradle-8.0.2-bin.zip && rm gradle-8.0.2-bin.zip && mv gradle-8.0.2 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"

USER ${JENKINS_USERID}



# shfmt
ENV SHFMT_PATH=/home/jenkins/bin
ENV SHFMT_VERSION=v3.3.1
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
    && cd "${SHFMT_PATH}" \
    && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
    && chmod a+x shfmt

# Ronn-NG
ENV PATH="$PATH:/home/jenkins/.local/share/gem/ruby/3.1.0/bin"
RUN gem install --user-install ronn-ng -v 0.10.1.pre3 && ronn --version
