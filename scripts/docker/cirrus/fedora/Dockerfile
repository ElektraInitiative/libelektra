FROM fedora:33

RUN dnf upgrade --refresh -y && dnf install -y \
        augeas-devel \
        bison \
        bison-devel \
        boost-devel \
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
        java-1.8.0-openjdk-devel \
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
        maven \
        ninja-build \
        openssl-devel \
        procps-ng \
        python3-devel \
        qt5-qtbase-devel \
        ruby-devel \
        rubygem-test-unit \
        swig \
        valgrind \
        xerces-c-devel \
        yajl-devel \
        yaml-cpp-devel \
        zlib-devel \
    && dnf clean all -y

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.10.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

