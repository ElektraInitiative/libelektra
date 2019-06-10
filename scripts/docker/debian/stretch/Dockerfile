FROM debian:stretch

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update \
    && apt-get -y install \
        curl \
        build-essential \
        autotools-dev \
        automake \
        cmake \
        pkg-config \
        ruby \
        ruby-dev \
        python-all \
        python-pip \
        python3-all \
        libpython3-dev \
        dh-exec \
        dh-lua \
        liblua5.3-dev \
        tclcl-dev \
        libaugeas-dev \
        libbotan1.10-dev \
        libgpgme-dev \
        libyajl-dev \
        git \
        libgit2-dev \
        libgtest-dev \
        libboost-all-dev \
        libssl-dev \
        libdbus-1-dev \
        libpcre3-dev \
        libpcre++-dev \
        libglib2.0-dev \
        libxerces-c-dev \
        qtbase5-dev \
        qtdeclarative5-dev \
        libmarkdown2-dev \
        discount \
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
        mingw-w64 \
        wine \
        llvm \
        icheck \
        devscripts \
        lintian \
        diffutils \
        patch \
        patchutils \
        git-buildpackage \
        reprepro \
        doxygen \
        graphviz \
        gawk \
        lcov \
        valgrind \
        ed \
        dnsutils \
        virtualenv \
        bison \
        uuid-dev \
        ninja-build \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install hspec QuickCheck

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

# ANTLR
ARG ANTLR_VERSION=4.7.2
RUN cd /usr/local/lib \
    && curl -o antlr.jar -L https://www.antlr.org/download/antlr-${ANTLR_VERSION}-complete.jar \
    && cd /usr/local/bin \
    && printf '\
#!/usr/bin/env sh\n\
CLASSPATH=.:/usr/local/lib/antlr.jar exec java -jar /usr/local/lib/antlr.jar "$@"' > antlr4 \
    && printf '\
#!/usr/bin/env sh\n\
java -classpath .:/usr/local/lib/antlr.jar org.antlr.v4.gui.TestRig "$@"' > grun \
    && chmod a+x antlr4 grun
# ANTLR C++ runtime
RUN cd /tmp \
    && git clone --branch ${ANTLR_VERSION} --depth 1 https://github.com/antlr/antlr4.git \
    && cd antlr4/runtime/Cpp \
    && mkdir build \
    && cd build \
    && cmake -GNinja -DANTLR_JAR_LOCATION=/usr/local/lib/antlr.jar -DCMAKE_BUILD_TYPE=Release .. \
    && ninja \
    && ninja install \
    && cd /tmp \
    && rm -r antlr4

# PEGTL
ARG PEGTL_VERSION=2.7.1
RUN cd /tmp \
    && git clone --branch ${PEGTL_VERSION} --depth 1 https://github.com/taocpp/PEGTL.git \
    && cp -R PEGTL/include/tao /usr/local/include \
    && rm -rf PEGTL

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

# yaml-cpp
RUN cd /tmp \
    && git clone --branch yaml-cpp-0.6.2 --depth 1 https://github.com/jbeder/yaml-cpp.git \
    && cd yaml-cpp \
    && mkdir build \
    && cd build \
    && cmake -GNinja -DYAML_CPP_BUILD_TESTS=OFF -DBUILD_SHARED_LIBS=ON .. \
    && ninja \
    && ninja install \
    && cd ../.. \
    && rm -r yaml-cpp

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
USER ${JENKINS_USERID}

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
RUN pip install cmake-format[yaml]==0.4.5

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
# Workaround for issue [#2139](http://issues.libelektra.org/2139)
RUN mkdir -p /home/jenkins/.cabal/lib
