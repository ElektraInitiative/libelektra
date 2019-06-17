FROM debian:stretch
ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
    cmake git build-essential curl \
    libboost-all-dev \
    libpcre3-dev \
    zlib1g-dev \
    libgcrypt11-dev \
    libicu-dev \
    python \
    libssl-dev \
    libyajl-dev \
    autoconf \
    automake \
    pkg-config \
    net-tools \
    && rm -rf /var/lib/apt/lists/*

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

ARG PARALLEL=2
WORKDIR /app/deps
ARG CPPCMS_VERSION="1.2.0"
RUN curl -o cppcms-${CPPCMS_VERSION}.tar.bz -L \
        "https://sourceforge.net/projects/cppcms/files/cppcms/${CPPCMS_VERSION}/cppcms-${CPPCMS_VERSION}.tar.bz2/download" \
    && tar -xjvf cppcms-${CPPCMS_VERSION}.tar.bz \
    && mkdir cppcms-${CPPCMS_VERSION}/build \
    && cd cppcms-${CPPCMS_VERSION}/build \
    && cmake .. \
    && make -j ${PARALLEL} \
    && make install \
    && cd /app/deps \
    && rm -Rf cppcms-${CPPCMS_VERSION}
ARG JANSSON_VERSION="2.11"
RUN curl -O http://www.digip.org/jansson/releases/jansson-${JANSSON_VERSION}.tar.gz \
    && tar -xvzf jansson-${JANSSON_VERSION}.tar.gz \
    && cd jansson-${JANSSON_VERSION} \
    && mkdir build && cd build \
    && cmake .. \
    && make -j ${PARALLEL} \
    && make check \
    && make install \
    && cd /app/deps \
    && rm -Rf jansson-${JANSSON_VERSION}
ARG LIBJWT_VERSION="1.9.0"
RUN curl -o libjwt-v${LIBJWT_VERSION}.tar.gz -L \
        "https://github.com/benmcollins/libjwt/archive/v${LIBJWT_VERSION}.tar.gz" \
    && tar -xvzf libjwt-v${LIBJWT_VERSION}.tar.gz \
    && cd libjwt-${LIBJWT_VERSION}  \
    && autoreconf -i \
    && ./configure \
    && make -j ${PARALLEL} all \
    && make install \
    && cd /app/deps \
    && rm -Rf libjwt-${LIBJWT_VERSION}

# start build of kdb
ENV C_FLAGS="-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-protector-strong -Wstack-protector -fPIE -pie"
WORKDIR /app/kdb
ADD . /app/kdb/
RUN ldconfig \
    && mkdir build \
    && cd build \
    && cmake -DENABLE_ASAN=ON -DBUILD_FULL=OFF -DBUILD_SHARED=ON \
             -DBUILD_STATIC=OFF -DBUILD_DOCUMENTATION=OFF \
             -DINSTALL_SYSTEM_FILES=OFF \
             -DPLUGINS="ALL;-EXPERIMENTAL;-fstab;-ruby;-lua;-python;-xerces;-yamlcpp;-python2;file;camel;yajl" \
             -DTOOLS="kdb;rest-backend" \
             -DCMAKE_C_FLAGS="$C_FLAGS" \
             -DCMAKE_CXX_FLAGS="$C_FLAGS" \
             -DCMAKE_EXE_LINKER_FLAGS="-Wl,-z,now -Wl,-z,relro" \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL} -LE kdbtests \
    && make install


FROM debian:stretch
ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

COPY --from=0 /usr/local /usr/local
RUN echo '/usr/local/lib/elektra/' > /etc/ld.so.conf.d/elektra.conf \
    && ldconfig
RUN apt-get update && apt-get install -y \
         libasan3 \
         libubsan0 \
         libboost-system1.62.0 \
         libboost-filesystem1.62.0 \
         libboost-thread1.62.0 \
         libssl1.1 \
         libicu57 \
         libyajl2 \
         pwgen \
    && rm -rf /var/lib/apt/lists/*

# prepare
# asan errors in mount-rest-backend-config and run-rest-backend
RUN kdb global-mount \
    && kdb mount-rest-backend-config || /bin/true \
    && kdb set -N system /sw/elektra/restbackend/#0/current/backend/jwt/encryption/secret `pwgen -1cns 30` \
    && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/api "http" \
    && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/ip "0.0.0.0" \
    && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/port 8080 \
    && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/http/script_names/#0 "/" \
    && kdb set '/sw/elektra/restbackend/#0/current/cppcms/daemon/enable' '0'\
    && kdb set '/sw/elektra/restbackend/#0/current/cppcms/logging/level' 'debug'

ENTRYPOINT ["kdb"]
CMD ["run-rest-backend"]
EXPOSE 8080
