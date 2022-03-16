FROM fedora:35

RUN dnf upgrade --refresh -y && dnf install -y \
        augeas-devel \
        bison \
        bison-devel \
        cmake \
        curl \
        dbus-devel \
        diffutils \
        file \
        findutils \
        flex \
        flex-devel \
        gcc-c++ \
        git \
        glib2 \
        gpgme-devel \
        java-11-openjdk-devel \
        jna \
        libasan \
        libcurl-devel \
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
        ruby-devel \
        rubygem-test-unit \
        swig \
        unzip \
        valgrind \
        wget \
        which \
        xerces-c-devel \
        yajl-devel \
        yaml-cpp-devel \
        zlib-devel \
    && dnf clean all -y

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.11.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

# download and install gradle
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-7.4-bin.zip && unzip gradle-7.4-bin.zip && rm gradle-7.4-bin.zip && mv gradle-7.4 /opt/gradle
RUN alternatives --auto java && alternatives --auto javac
ENV PATH="${PATH}:/opt/gradle/bin" \
    JAVA_HOME="/usr/lib/jvm/java"
