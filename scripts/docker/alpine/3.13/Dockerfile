FROM alpine:3.13.5

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
        ronn \
        yaml-cpp-dev

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
