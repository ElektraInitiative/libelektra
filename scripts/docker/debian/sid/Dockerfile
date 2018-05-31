FROM debian:sid

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        curl \
        build-essential \
        clang-5.0 \
        clang-format-5.0 \
        autotools-dev \
        automake \
        cmake \
        pkg-config \
        bison \
        ruby-dev \
        python-dev \
        python-pip \
        python3-dev \
        libpython3-dev \
        dh-lua \
        liblua5.3-dev \
        tclcl-dev \
        libaugeas-dev \
        libbotan1.10-dev \
        libyajl-dev \
        git \
        libgit2-dev \
        libboost-all-dev \
        libssl-dev \
        libdbus-1-dev \
        libpcre3-dev \
        libpcre++-dev \
        libglib2.0-dev \
        libxerces-c-dev \
        libyaml-cpp-dev \
        swig3.0 \
        libuv1-dev \
        libev-dev \
        libzmq3-dev \
        ghc \
        ghc-dynamic \
        cabal-install \
        alex \
        happy \
        c2hs \
        checkinstall \
        openjdk-8-jdk \
        maven \
        gobject-introspection \
        libgirepository1.0-dev \
        systemd \
        libsystemd-dev \
        llvm \
        sloccount \
        gawk \
        lcov \
        icheck \
        valgrind \
        moreutils \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install hspec QuickCheck
RUN pip install --upgrade cmake_format==0.4.1 pyyaml

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=4e4df226fc197c0dda6e37f5c8c3845ca1e73a49
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

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers
ARG JENKINS_GROUPID
RUN groupadd \
    -g ${JENKINS_GROUPID} \
    jenkins

ARG JENKINS_USERID
RUN useradd \
    --create-home \
    --uid ${JENKINS_USERID} \
    --gid ${JENKINS_GROUPID} \
    --shell "/bin/bash" \
    jenkins

USER ${JENKINS_USERID}

# Handle Haskell dependencies
ENV HASKELL_SHARED_SANDBOX /home/jenkins/elektra-cabal-sandbox
RUN mkdir -p $HASKELL_SHARED_SANDBOX \
    && cd $HASKELL_SHARED_SANDBOX \
    && cabal update \
    && cabal sandbox init \
    && cabal install 'base >=4.9 && <4.12' 'containers >=0.5 && <0.6' \
        'directory >=1.2 && <1.4' 'process >=1.4 && <1.7' 'binary >=0.8 && <0.9' \
        'haskell-src-exts-any' 'pretty -any' 'hint >=0.7.0 && <0.8.0' 'temporary -any' \
        'exceptions -any' 'text -any' 'simple-logger -any' 'megaparsec -any' \
        'hspec -any' 'QuickCheck-any' --avoid-reinstalls
