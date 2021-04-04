FROM fedora:32

RUN dnf upgrade -y && dnf install -y \
        augeas-devel \
        bison \
        bison-devel \
        boost-devel \
        cmake \
        curl \
        dbus-devel \
        diffutils \
        dnf-plugins-core \
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
        qt5-devel \
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
    && dnf debuginfo-install -y \
        bison \
        boost \
        flex \
        glib2 \
        glibc \
        gpgme \
        libgcrypt \
        ruby-libs \
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

# download and install gradle
RUN cd /tmp && wget https://services.gradle.org/distributions/gradle-6.8.3-bin.zip && unzip gradle-6.8.3-bin.zip && rm gradle-6.8.3-bin.zip && mv gradle-6.8.3 /opt/gradle
ENV PATH "${PATH}:/opt/gradle/bin"
RUN echo "export JAVA_HOME=$(dirname $(dirname $(readlink $(readlink $(which javac)))))" >> /etc/profile.d/java.sh
RUN echo "export PATH=$PATH:$JAVA_HOME/bin" >> /etc/profile.d/java.sh
RUN echo "CLASSPATH=.:$JAVA_HOME/jre/lib:$JAVA_HOME/lib:$JAVA_HOME/lib/tools.jar" >> /etc/profile.d/java.sh
RUN source /etc/profile.d/java.sh

USER ${JENKINS_USERID}

# extend PATH for user jenkins to include gradle
RUN echo "export PATH=${PATH}:/opt/gradle/bin" >> /home/jenkins/.bashrc

# Set git config
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
