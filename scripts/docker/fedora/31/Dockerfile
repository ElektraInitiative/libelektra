FROM fedora:31

RUN dnf upgrade -y && dnf install -y \
        augeas-devel \
        boost-devel \
        botan-devel \
        cmake \
        curl \
        diffutils \
        file \
        findutils \
        gcc-c++ \
        git \
        glib2 \
        gpgme-devel \
        java-1.8.0-openjdk-devel \
        jna \
        libcurl-devel \
        libgcrypt-devel \
        libgit2-devel \
        libxml2-devel \
        lua-devel \
        make \
        maven \
        ninja-build \
        openssl-devel \
        procps-ng \
        python3-devel \
        qt5-devel \
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

