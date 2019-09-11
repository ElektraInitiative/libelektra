FROM ubuntu:artful

RUN apt-get -qq update && apt-get -qq -y install \
	curl \
	build-essential \
	clang-5.0 clang-3.8 \
	autotools-dev \
	automake \
	cmake \
	pkg-config \
	doxygen \
	graphviz \
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
	libgpgme-dev \
	libdbus-1-dev \
	libpcre3-dev \
	libpcre++-dev \
	libglib2.0-dev \
	swig3.0 \
	libuv1-dev \
	libev-dev \
	libzmq3-dev \
	checkinstall \
	valgrind \
&& gem install ronn \
&& rm -rf /var/lib/apt/lists/*

COPY buildelektra.sh /bin/buildelektra
