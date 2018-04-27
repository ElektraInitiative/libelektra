FROM ubuntu:xenial

RUN apt-get update && apt-get -y install \
        locales \
        curl \
        build-essential \
        autotools-dev \
        automake \
        cmake \
        pkg-config \
        bison \
        ruby-dev \
        python-dev \
        python3-dev \
        libpython3-dev \
        liblua5.3-dev \
        tclcl-dev \
        libaugeas-dev \
        libyajl-dev \
        libgit2-dev \
        libboost-all-dev \
        libssl-dev \
        libdbus-1-dev \
        libpcre3-dev \
        libpcre++-dev \
        libxerces-c-dev \
        valgrind \
        checkinstall \
        swig \
    && rm -rf /var/lib/apt/lists/*

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
