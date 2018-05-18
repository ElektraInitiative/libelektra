FROM debian:stretch

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update \
    && apt-get -y install \
        curl \
        build-essential \
        clang \
        autotools-dev \
        automake \
        cmake \
        pkg-config \
        bison \
        ruby \
        ruby-dev \
        python-all \
        python3-all \
        libpython3-dev \
        dh-exec \
        dh-lua \
        liblua5.3-dev \
        tclcl-dev \
        libaugeas-dev \
        libbotan1.10-dev \
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
        libyaml-cpp-dev \
        qtbase5-dev \
        qtdeclarative5-dev \
        libmarkdown2-dev \
        discount \
        swig3.0 \
        libuv1-dev \
        ghc \
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
        icheck \
        valgrind \
    && rm -rf /var/lib/apt/lists/*

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

RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'

# Handle Haskell dependencies
ENV HASKELL_SHARED_SANDBOX $HOME/elektra-cabal-sandbox
RUN cd $HOME \
    && cabal update \
    && cabal sandbox init --sandbox $HOME/elektra-cabal-sandbox \
    && cabal install 'containers >=0.4 && <0.6' 'base >=4.7 && <5' 'directory >=1.1 && <1.4' \
        'process >=1.2 && <1.7' 'filepath >=1.3 && <1.5' 'base -any' 'hspec -any' 'QuickCheck -any'
