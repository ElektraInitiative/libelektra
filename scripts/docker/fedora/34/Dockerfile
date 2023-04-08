FROM fedora:34

RUN dnf upgrade --refresh -y && dnf install -y \
        augeas-devel \
        bison \
        bison-devel \
        cmake \
        curl \
        dbus-devel \
    	dbus-x11 \
        devscripts \
        diffutils \
        discount \
        dnf-plugins-core \
        fedora-packager \
        file \
        findutils \
        flex \
        flex-devel \
        gcc-c++ \
        ghostscript \
        git \
        glib2 \
        gpgme-devel \
        graphviz \
        java-11-openjdk-devel \
        jna \
        libasan \
        libcurl-devel \
        libdrm-devel \
        libev-devel \
        libgcrypt-devel \
        libgit2-devel \
        libmarkdown-devel \
        libubsan \
        libuv-devel \
        libxml2-devel \
        llvm \
        lua-devel \
        make \
        ninja-build \
        openssl-devel \
        procps-ng \
        python3-devel \
        qt5-qtbase-devel \
        qt5-qtdeclarative-devel \
        qt5-qtsvg-devel \
        rpmdevtools \
        ruby-devel \
        rubygem-test-unit \
        strace \
        swig \
        systemd-devel \
        unzip \
        valgrind \
        wget \
        which \
        xerces-c-devel \
        xfconf-devel \
        yajl-devel \
        yaml-cpp-devel \
        zeromq-devel \
        zlib-devel \
        python3-pip \
    	golang \
    && dnf debuginfo-install -y \
        bison \
        flex \
        glib2 \
        glibc \
        gpgme \
        libgcrypt \
        ruby-libs \
    && dnf clean all -y

# Build dependency for libelektra-fuse
RUN pip3 install wheel

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
RUN alternatives --auto java && alternatives --auto javac
ENV PATH="${PATH}:/opt/gradle/bin" \
    JAVA_HOME="/usr/lib/jvm/java"

USER ${JENKINS_USERID}

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
