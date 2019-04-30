FROM debian:stretch

ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get -y install \
        cmake git build-essential curl \
        libyajl-dev \
    && curl -sL https://deb.nodesource.com/setup_9.x | bash - \
    && apt-get install -y nodejs \
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
# start build of kdb
ENV C_FLAGS="-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-protector-strong -Wstack-protector -fPIE -pie"
WORKDIR /app/kdb
ADD . /app/kdb/
RUN mkdir build \
    && cd build \
    && cmake -DENABLE_ASAN=ON -DBUILD_FULL=OFF -DBUILD_SHARED=ON \
             -DBUILD_STATIC=OFF -DBUILD_DOCUMENTATION=OFF \
             -DINSTALL_SYSTEM_FILES=OFF \
             -DPLUGINS="ALL;-EXPERIMENTAL;-fstab;-ruby;-lua;-python;-xerces;-yamlcpp;-python2;file;camel;yajl" \
             -DTOOLS="kdb;rest-frontend" \
             -DCMAKE_C_FLAGS="$C_FLAGS" \
             -DCMAKE_CXX_FLAGS="$C_FLAGS" \
             -DCMAKE_EXE_LINKER_FLAGS="-Wl,-z,now -Wl,-z,relro" \
             .. \
    && make -j ${PARALLEL} \
    && ctest -T Test --output-on-failure -j ${PARALLEL} -LE kdbtests \
    && make install \
    && echo '/usr/local/lib/elektra/' > /etc/ld.so.conf.d/elektra.conf \
    && ldconfig

ARG BACKEND=https://restapi.libelektra.org/
ARG FRONTEND=https://libelektra.org/
RUN kdb global-mount \
    && kdb mount-rest-frontend-config \
    && kdb set -N system /sw/elektra/restfrontend/#0/current/backend/root "${BACKEND}" \
    && kdb set -N system /sw/elektra/restfrontend/#0/current/website/url "${URL}" \
    && kdb build-rest-frontend

FROM nginx:alpine
COPY --from=0 /usr/local/share/elektra/tool_data/rest-frontend/public \
              /usr/share/nginx/html
