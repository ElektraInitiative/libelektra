FROM alpine:3.15.4

RUN apk update \
    && apk add --no-cache \
        bison \
        build-base \
        cmake \
        curl \
        diffutils \
        file \
        flex \
        git \
        gtest-dev \
        ninja \
        yaml-cpp-dev \
	py3-pip

# Build dependency for libelektra-fuse
RUN pip3 install wheel

# Google Test
ENV GTEST_ROOT=/usr/include/gtest

# Create User:Group
# The id is important as jenkins docker agents use the same id that is running
# on the slaves to execute containers
ARG JENKINS_USERID
RUN adduser \
    -u ${JENKINS_USERID} \
    -D \
    jenkins
USER ${JENKINS_USERID}

RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
    && git config --global user.name 'Jenkins'
