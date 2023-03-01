FROM archlinux:base

RUN pacman -Sy --noconfirm \
    awk \
    cmake \
    diffutils \
    gcc \
    lua \
    make \
    ninja \
    rubygems \
    swig \
    tar \
    python-pip

# Dependency for libelektra-fuse
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
USER ${JENKINS_USERID}

# Ronn-NG
ENV PATH="$PATH:/home/jenkins/.local/share/gem/ruby/3.0.0/bin"
RUN gem install ronn-ng -v 0.10.1.pre1 && ronn --version
