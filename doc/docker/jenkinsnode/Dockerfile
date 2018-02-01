FROM debian:stretch

ARG SWIGVERSION="3.0.11"

# install dependencies
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install \
  apt-utils \
  curl \
  build-essential \
  clang \
  autotools-dev \
  automake \
  cmake \
  pkg-config \
  doxygen \
  graphviz \
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
  libglib2.0-dev \
  swig \
  libyaml-cpp-dev \
  libuv1-dev \
  libxerces-c-dev \
  checkinstall \
  valgrind \
  devscripts \
  dh-exec \
  libmarkdown2-dev \
  discount \
  dh-lua \
  python-all \
  python3-all \
  libgtest-dev \
  ruby-ronn \
  libgcrypt20-dev \
  libbotan1.10-dev \
  swig3.0 \
  libsystemd-dev \
  openjdk-8-jdk-headless \
  ghc \
  ghc-dynamic \
  cabal-install \
  alex \
  happy \
  c2hs \
  openssh-server \
  maven \
  git \
  libcurl4-gnutls-dev
RUN cabal update && \
  gem install ronn && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

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
  echo "[user]\nname = Jenkins Buildbot\nemail = <your-email>" >> /home/jenkins/.gitconfig

# setup the run- utilities
COPY run-make /usr/local/bin/run-make
COPY run-make-env /usr/local/bin/run-make-env
COPY run-nice /usr/local/bin/run-nice
RUN chmod a+x /usr/local/bin/run-*

# start the ssh server
EXPOSE 22
RUN service ssh start
CMD ["/usr/sbin/sshd", "-D", "-e"]
