FROM debian:stretch

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        automake \
        autotools-dev \
        bison \
        build-essential \
        checkinstall \
        cmake \
        curl \
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
        git \
        git-buildpackage \
        gobject-introspection \
        graphviz \
        icheck \
        lcov \
        libaugeas-dev \
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
        libssl-dev \
        libsystemd-dev \
        libuv1-dev \
        libxerces-c-dev \
        libyajl-dev \
        libzmq3-dev \
        lintian \
        llvm \
        mingw-w64 \
        ninja-build \
        openjdk-8-jdk \
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
        swig3.0 \
        systemd \
        tclcl-dev \
        unzip \
        uuid-dev \
        valgrind \
        virtualenv \
        wine \
    && rm -rf /var/lib/apt/lists/*

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.11.0
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
    && pip install "urllib3==1.22" \
    && pip install pyyaml \
    && pip install cpp-coveralls \
    && deactivate
ENV PATH="${PATH}:${COVERALLS_VIRTUALENV_PATH}/bin"
