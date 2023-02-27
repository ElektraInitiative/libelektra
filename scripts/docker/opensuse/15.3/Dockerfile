FROM opensuse/leap:15.3

RUN zypper update -y && zypper install -y \
        augeas-devel \
        augeas-lenses \
        bison \
        cmake \
        curl \
        dbus-1-devel \
        devscripts \
        diffutils \
        discount \
        dnf-plugins-core \
        file \
        findutils \
        flex \
        gcc-c++ \
        ghostscript \
        git \
        glib2 \
        glib2-devel \
        gzip \
        gpgme-devel \
        graphviz \
        graphviz-gd \
        java-11-openjdk-devel \
        lato-fonts \
        libcurl-devel \
        libdrm-devel \
        libev-devel \
        libgcrypt-devel \
        libgit2-devel \
        libmarkdown-devel \
        libqt5-qtbase-devel \
        libqt5-qtdeclarative-devel \
        libqt5-qtsvg-devel \
        libuv-devel \
        libxerces-c-devel \
        libxfconf-devel \
        libxml2-devel \
        llvm \
        lua-devel \
        make \
        ninja \
        openssl-devel \
        procps \
        python3-devel \
        rpm-build \
        rpmdevtools \
        ruby-devel \
        strace \
        swig \
        systemd-devel \
        tar \
        unzip \
        valgrind \
        wget \
        which \
        xerces-c \
        libyajl-devel \
        yaml-cpp-devel \
        zeromq-devel \
        zlib-devel \
        python3-pip

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.13.0
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
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-8.0.1-bin.zip && unzip gradle-8.0.1-bin.zip && rm gradle-8.0.1-bin.zip && mv gradle-8.0.1 /opt/gradle
RUN alternatives --auto java && alternatives --auto javac
ENV PATH="${PATH}:/opt/gradle/bin" \
    JAVA_HOME="/etc/alternatives/jre"

USER ${JENKINS_USERID}

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
