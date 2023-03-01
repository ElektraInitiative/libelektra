# syntax = docker/dockerfile:1.2
FROM debian:bullseye

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        cmake git build-essential curl \
        libyajl-dev \
    && curl -fsSL https://deb.nodesource.com/setup_16.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g npm \
    && rm -rf /var/lib/apt/lists/*

# Google Test
ENV GTEST_ROOT=/opt/gtest
ARG GTEST_VER=v1.13.0
RUN mkdir -p ${GTEST_ROOT} \
    && cd /tmp \
    && curl -o gtest.tar.gz \
      -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
    && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
    && rm gtest.tar.gz

ARG PARALLEL=8
# start build of kdb
ENV C_FLAGS="-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-protector-strong -Wstack-protector -fPIE -pie"
WORKDIR /app/kdb
ADD . /app/kdb/
RUN --mount=type=tmpfs,target=/tmp \
    --mount=type=tmpfs,target=/root/.cache/elektra \
    --mount=type=tmpfs,target=/root/.config \
    mkdir -p build \
    && cd build \
    && cmake -DBUILD_FULL=OFF -DBUILD_SHARED=ON \
             -DBUILD_STATIC=OFF -DBUILD_DOCUMENTATION=OFF \
             -DPLUGINS="ALL;-EXPERIMENTAL;-fstab;-ruby;-lua;-python;-xerces;-yamlcpp;file;yajl" \
             -DTOOLS="kdb;website" \
             -DCMAKE_C_FLAGS="$C_FLAGS" \
             -DCMAKE_CXX_FLAGS="$C_FLAGS" \
             -DCMAKE_EXE_LINKER_FLAGS="-Wl,-z,now -Wl,-z,relro" \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL} -LE kdbtests \
    && make install \
    && echo '/usr/local/lib/elektra/' > /etc/ld.so.conf.d/elektra.conf \
    && ldconfig

ARG URL=https://www.libelektra.org/
RUN kdb mount-website-config \
    && kdb set system:/sw/elektra/websitefrontend/#0/current/website/url "${URL}" \
    && kdb build-website

# FROM nginx:alpine
# COPY --from=0 /usr/local/share/elektra/tool_data/website/public \
#               /usr/share/nginx/html
