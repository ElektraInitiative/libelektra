FROM ubuntu:xenial

ARG SWIGVERSION="3.0.11"

RUN apt-get -qq update && apt-get -qq -y install \
	curl \
	build-essential \
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
	checkinstall \
&& gem install ronn \
&& rm -rf /var/lib/apt/lists/*

# install Swig 3
RUN cd /tmp \
&& curl -fsS "https://codeload.github.com/swig/swig/tar.gz/rel-${SWIGVERSION}" \
| tar xz \
&& cd "swig-rel-${SWIGVERSION}" \
&& ./autogen.sh \
&& ./configure \
&& make \
&& make install \
&& rm -rf "/tmp/swig-rel-${SWIGVERSION}"

COPY buildelektra.sh /bin/buildelektra
