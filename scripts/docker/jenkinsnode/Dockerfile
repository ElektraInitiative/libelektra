FROM debian:stretch

# install dependencies
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install \
  build-essential \
  clang \
  cmake \
  ruby-dev \
  python-dev \
  libaugeas-dev \
  libyajl-dev \
  libgit2-dev \
  libboost-all-dev \
  libssl-dev \
  libdbus-1-dev \
  libpcre3-dev \
  libpcre++-dev \
  libglib2.0-dev \
  libxerces-c-dev \
  swig \
  valgrind \
  libmarkdown2-dev \
  discount \
  dh-lua \
  python-all \
  python3-all \
  ruby-ronn \
  libgcrypt20-dev \
  libgpgme-dev \
  libev-dev \
  libuv1-dev \
  libsystemd-dev \
  libuv1-dev \
	libzmq3-dev \
  openjdk-8-jdk-headless \
  openssh-server \
  maven \
  git \
  libcurl4-gnutls-dev
RUN apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# TODO use Elektra for the configuration steps below

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.10.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz


# setup the ssh server
RUN sed --in-place 's/^\(PermitRootLogin\|UsePAM\|UseDNS\)/#\1/' /etc/ssh/sshd_config && \
  echo "" >> /etc/ssh/sshd_config && \
  echo "# Custom changes from `date`" >> /etc/ssh/sshd_config && \
  echo "PermitRootLogin no" >> /etc/ssh/sshd_config && \
  echo "UsePAM no" >> /etc/ssh/sshd_config && \
  echo "UseDNS no" >> /etc/ssh/sshd_config && \
  echo "SSH daemon config updated"

# setup jenkins prerequisites
RUN echo "\n\n\n\n\nY" | adduser --quiet --disabled-password jenkins && \
  echo "jenkins:<password>" | chpasswd && \
  mkdir /home/jenkins/.m2/ && \
  chown -R jenkins:jenkins /home/jenkins/.m2/ && \
  mkdir /home/jenkins/libelektra && \
  echo "[user]\nname = Jenkins Buildbot\nemail = bot@libelektra.org" >> /home/jenkins/.gitconfig

# setup the run- utilities
COPY run-make /usr/local/bin/run-make
COPY run-make-env /usr/local/bin/run-make-env
COPY run-nice /usr/local/bin/run-nice
RUN chmod a+x /usr/local/bin/run-*

# start the ssh server
EXPOSE 22
RUN service ssh start
CMD ["/usr/sbin/sshd", "-D", "-e"]
