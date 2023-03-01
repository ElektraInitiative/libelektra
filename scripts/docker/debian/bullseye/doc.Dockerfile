FROM debian:bullseye

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
    cmake git build-essential curl

RUN apt-get -y install \
        doxygen \
        graphviz \
        ruby \
        ruby-dev \
        sloccount \
        texlive-latex-base \
        texlive-latex-recommended \
        texlive-latex-extra \
        texlive-fonts-recommended \
        texlive-fonts-extra \
        texlive-science \
    && gem install apiaryio \
    && rm -rf /var/lib/apt/lists/*

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
ENV PATH="$PATH:/home/jenkins/.local/share/gem/ruby/2.7.0/bin"
RUN gem install --user-install ronn-ng -v 0.10.1.pre1 && ronn --version
