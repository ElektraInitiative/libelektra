FROM centos:7

# We need epel to get cmake3
RUN curl -O http://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm && \
    rpm -ivh epel-release-latest-7.noarch.rpm && \
    rm epel-release-latest-7.noarch.rpm

# Install all core deps
RUN yum install -y cmake3 \
                   gcc \
                   gcc-c++ \
                   make

# Install doc deps
RUN yum install -y doxygen docbook-style-xsl graphviz ruby ruby-devel && \
    gem install ronn

# Install optional deps
# TODO split according to plugin
RUN yum install -y augeas-devel \
                   boost-devel \
                   libdb-devel \
                   GConf2-devel \
                   libxml2-devel \
                   yajl-devel \
                   libcurl-devel \
                   libgit2-devel \
                   lua-devel \
                   python34-devel \
                   python-devel \
                   java-1.8.0-openjdk-devel \
                   jna \
                   byacc

# build swig 3 from source
ARG SWIG_VERSION=3.0.12
ENV SWIG_FILE=swig-$SWIG_VERSION.tar.gz
RUN mkdir -p /deps/swig3 && \
    cd /deps/swig3 && \
    curl -L -O http://prdownloads.sourceforge.net/swig/$SWIG_FILE && \
    tar -zxvf $SWIG_FILE && \
    rm $SWIG_FILE && \
    cd swig-$SWIG_VERSION && \
    ./configure && make && make install
