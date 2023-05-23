FROM debian:bullseye

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN dpkg --add-architecture i386 \
    && apt-get update \
    && apt-get -y install \
        curl \
        build-essential \
        autotools-dev \
        automake \
        cmake \
        pkgconf \
        gcc-multilib \
        g++-multilib \
        file \
	python3-pip \
	python3-wheel \
    && rm -rf /var/lib/apt/lists/*

RUN ln -sf pkgconf /usr/bin/pkg-config

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.12.1
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
