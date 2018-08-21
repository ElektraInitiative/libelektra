FROM ubuntu:xenial

RUN apt-get update && apt-get -y install \
        locales \
        curl \
        build-essential \
        autotools-dev \
        automake \
        cmake \
        pkg-config \
        bison \
        ruby-dev \
        python-dev \
        python3-dev \
        libpython3-dev \
        liblua5.3-dev \
        tclcl-dev \
        libaugeas-dev \
        libyajl-dev \
        git \
        libgit2-dev \
        libboost-all-dev \
        libssl-dev \
        libdbus-1-dev \
        libpcre3-dev \
        libpcre++-dev \
        libxerces-c-dev \
        valgrind \
        checkinstall \
        swig \
        libgmp-dev \
        wget \
        happy \
        alex \
        c2hs \
    && rm -rf /var/lib/apt/lists/*

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=4e4df226fc197c0dda6e37f5c8c3845ca1e73a49
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

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

# Handle Haskell dependencies
# We use ghc8.2.2 from the haskell platform website here, as xenial only provides ghc7
RUN wget https://haskell.org/platform/download/8.2.2/haskell-platform-8.2.2-unknown-posix--core-x86_64.tar.gz \
    && tar -xf haskell-platform-8.2.2-unknown-posix--core-x86_64.tar.gz \
    && ./install-haskell-platform.sh

USER ${JENKINS_USERID}

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
